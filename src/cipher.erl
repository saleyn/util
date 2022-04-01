-module(cipher).

-export([encrypt/2, decrypt/2, make_key/1, make_key_base64/1]).
-export([encrypt_base64/2, decrypt_base64/2]).
-export([gpg_key_decode/1]).

-define(AAD, <<"AES256GCM">>).

encrypt(Text, KeyPlainText) when (is_list(Text) orelse is_binary(Text)), is_list(KeyPlainText) ->
  Key = make_key(KeyPlainText),
  {encrypt(Text, Key), Key};

encrypt(Text, Key) when (is_list(Text) orelse is_binary(Text)), is_binary(Key), byte_size(Key) =:= 32 ->
  Data = iolist_to_binary([<<(erlang:system_time(microsecond)):64/integer>>, Text]),
  IV   = crypto:strong_rand_bytes(16), % create random Initialisation Vector
  {Ciphertext, Tag} = crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, Data, ?AAD, true),
  <<IV/binary, Tag/binary, Ciphertext/binary>>. % "return" iv with the cipher tag & ciphertext

decrypt(_Encrypted = <<IV:16/binary, Tag:16/binary, Cipher/binary>>, Key) when byte_size(Key) =:= 32 ->
  <<_Time:64/integer, Text/binary>> =
    crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, Cipher, ?AAD, Tag, false),
  Text.

make_key(PlainKey) when is_list(PlainKey); is_binary(PlainKey) ->
  crypto:hash(sha256, PlainKey).


encrypt_base64(Text, KeyBase64) ->
  base64:encode(encrypt(Text, base64:decode(KeyBase64))).

decrypt_base64(EncryptedBase64, KeyBase64) when is_list(EncryptedBase64) ->
  decrypt_base64(list_to_binary(EncryptedBase64), KeyBase64);
decrypt_base64(EncryptedBase64, KeyBase64) when is_binary(EncryptedBase64) ->
  decrypt(base64:decode(EncryptedBase64), base64:decode(KeyBase64)).

make_key_base64(PlainKey) when is_list(PlainKey); is_binary(PlainKey) ->
  base64:encode(crypto:hash(sha256, PlainKey)).

%% @doc Decode PGP public key
-spec gpg_key_decode(string()|binary()) -> [public_key:pem_entry()].
gpg_key_decode(File) when is_list(File) ->
  {ok, B} = file:read_file(File),
  gpg_key_decode(B);
gpg_key_decode(Bin) when is_binary(Bin) ->
  B1 = re:replace(Bin, <<"-----BEGIN PGP (PUBLIC|PRIVATE) KEY BLOCK-----">>,
                       <<"-----BEGIN RSA \\1 KEY-----">>, [{return, binary}]),
  B2 = re:replace(B1,  <<"-----END PGP (PUBLIC|PRIVATE) KEY BLOCK-----">>,
                       <<"-----END RSA \\1 KEY-----">>,   [{return, binary}]),
  B3 = re:replace(B2,  <<"\n.+(\n-----END)">>,    <<"\\1">>, [{return, binary}]), % Remove checksum
  B4 = re:replace(B3,  <<"\nVersion:.+\n\r?\n">>, <<"\n">>,  [{return, binary}]), % Remove version
  public_key:pem_decode(B4).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(EUNIT).
run_test() ->
  Text  = <<"This is a test">>,
  Key   = make_key("Test"),
  Text  = cipher:decrypt(cipher:encrypt(Text, Key), Key),
  Key64 = base64:encode(Key),
  Text  = cipher:decrypt_base64(cipher:encrypt_base64(Text, Key64), Key64).

decode_gpg_key_test() ->
  Key   = <<"-----BEGIN PGP PUBLIC KEY BLOCK-----\r\n"
            "Version: GnuPG v1.4.11 (GNU/Linux)\r\n"
            "\r\n"
            "mQINBE9NBIQBEADMSzN6b0FaPP0rGiLDWKfH4ehN66Z0SAIynXm6lBHjmO69pNsm\r\n"
            "iIe4p1X9aXhr7EgEZKdbqevfXW7NuA/oL7Rbt+tzBT5nS2cYSHoZhUC/onVhJxUb\r\n"
            "drCR9NsBDZc1wZs+b95K3vWW91wMPYs4tn71DeeNgUVCcZOGofWltI7+etTVVIyG\r\n"
            "zMEW4lOiEwBgLee+5u3XyGsBbyhtWnbp8ZMewiXjo14w0UCMEQxvf55NjggAO/vR\r\n"
            "C+Czz5FQ7m9AnZwqIZJfaAw+aM81lmcXRZenGZ/H25H6WLq2DrVXnRjEyUvK+juj\r\n"
            "K6rTHcot6K4Cgvo3P47uZcze92c4KaI22jTePRB1qov8ygAQw8BPsaZ+cA3AQ2Zv\r\n"
            "4hjIyx2Qovs+vb1xWhk9Mgqyt0ToMZ6HNn386ICxaeYR8XY/rhf9ej0PJOS5c9Nx\r\n"
            "reeTdrYXeco27kp/x+nDSWzqy0MrSAAD19xCK1w8Eiwc9MmjAAX1yarQzUy/ICey\r\n"
            "eY6SaBfdK7x7A1ecUSu7mS7LmGFWB49Tx5u3ENXw9rU2hKbmGbbQsc+xCx5s7ZM1\r\n"
            "q0aATJsUO4fsNe+4/9cBXCSP+4P6irX1EfliqaySt0LEr0WYhIgNgYq8PB7yx4Cv\r\n"
            "0mOMv8lXBMYZs8NTrMJl2XHfi3egu8Ti14VChCklnWe95sZvKrk9/3BYxwARAQAB\r\n"
            "tCtMZW8gWm92aWMgKEluYWltYXRoaSkgPGxlby56b3ZpY0BnbWFpbC5jb20+iQI4\r\n"
            "BBMBAgAiBQJPTQSEAhsDBgsJCAcDAgYVCAIJCgsEFgIDAQIeAQIXgAAKCRDlo9AP\r\n"
            "+vpofbYuD/9luGWbbw0ia5EfsDArk+iAs55HdmgjtVrcEqASR92bSbXxUylvM41T\r\n"
            "Gd4nrM7Ri9XVgUSj6ZMnvPJihk08dkjWtoz83JQFdyJ9u7vIqs0LU4awrbNQ8l7P\r\n"
            "M3fVKOme/vwQQ7kyIWkIYB0bl+CDRnldYGyBbV0lzh25/eVnhE8AsDltHJjhwe17\r\n"
            "GVQPQfk/4B9SY0bTuAojZCUpMtI4cSBaI4v+xojewFt9B53oxW/KrLOvCVYUE9m2\r\n"
            "TiJrtWcQjYqrlY9Ku203LzIZWbsFt9NEM6I26ewL3Iqn687fLgdkXZ2TuomJEcY+\r\n"
            "5UxPIZfdiXqNG/nFLSII3v9kWA/f6ysXn7NTFx594+5KUqwjPke9ZFxi7gmyuRtK\r\n"
            "KQyz1P+fRpkHqdP+OdNASDZ66CUaOqJrea6N+HpGIdBgRVfaYezl0wy9QfPe/PV7\r\n"
            "ewYlP/nEpXDNpnr4cOvvD52VeoW9oiZEt0UFZ5iWEVepZlJ9uNq1QKkKDGHjBA/n\r\n"
            "Mgd/Mc/Bg6sTfs6gtKfiX5MoPhZd/WDonAZyimJMY23Dw31TuIjmEmO1MuNNxj6c\r\n"
            "Q2qWCVsQED6GgkPSRMwZ4+6Dh8DsLrt5xQp68I204XnT+ppgkXY/f4ALseRlOXeQ\r\n"
            "9TeqeMcq7lO9DVULHLfXELw5/ijN08BBToZBIaBPKKoGhEdWBl9P3bkCDQRPTQSE\r\n"
            "ARAAt9MpaK5TzTOzO/IenaHpfu0EoI4ZQSiWD4ZujvyuMEscdPQ4wDRMVwh66FAW\r\n"
            "jeRE4O/m1q4rt/qDKqa0VCSGmap9gj7iLZ9+Fu5n3jyentzmoe22ATO7jOITGbW2\r\n"
            "+0D56U3jCRRoSsY8ZSqECBLhsKbc7WIBX8yVxvpHYaqTOwM2+CIqDda5vgKeNxzk\r\n"
            "cVDgUBnYj1nNpklGCs3494IVQ8aLVo5nOm8J0vY/n/YK5sAf6RlK+MEXCEyMVoiG\r\n"
            "V4wo4fGLJXkhky5yL9vW+vmkUslw7mbAFRIC2KLg0CxXaGychsScpDlpmHkcxmsl\r\n"
            "riGbEaKe0kMOM4KkXdopoGB66T0sEftL+hJmrAVe3H4iyldh/d5Hf1ez670m4ZZ6\r\n"
            "gNdRCv3WJ72mZv4pqFH94NUksHvPrmot0B23ne6y/MqxYLIweIjJkD1ePxqpffMq\r\n"
            "Ktkq9ooH7SB2GAMihCWiY/orSi6bt670Y9P3pzrwolErCWxieX8dg3H04z5nA7J3\r\n"
            "6mqxQXLQvY6lXZYANOEKmm1qyoDgeBJoJdYUFbHrb83xcxQUqp8zikKneQ0rJfVG\r\n"
            "dj38smSNAMqOeQQwZlmKFVwmIu4ozqszgZ2oqWS7q2NJgg64dnQlA60VMebQ0e8d\r\n"
            "MMqy41VJ1FF8PCT1GGMsL+H4vdoZ+/wF2bhMQISWqFSVa5EAEQEAAYkCHwQYAQIA\r\n"
            "CQUCT00EhAIbDAAKCRDlo9AP+vpofVp4D/9WqB4h9T5kGLBvuGUebjSqBuv6XfUd\r\n"
            "q8vrvrMbSiLTLj2Gk58FjXdTPKCAuTkTtiYjMIXR0cF30uGRccM/tOMSp0xQYVT6\r\n"
            "ueBhDZHaWaAEr2408j7/+tVg5CaLO/dVVfxpHIJ+8Bf1YmRRMpDm94i8X5j4rxPv\r\n"
            "GNaOa6CgqWGlXsqFUw19OqXI5pK+hBH/GEpVPawr8/JLauc24ovt76gLGXDHrmKV\r\n"
            "aeryKzy3TWikj3cq8Mdj2mKqNwkn1uu89j3xvxbq6gxX7lGX8pCLS2W6a7PYDaXf\r\n"
            "no1/C31//Shn26LD9YfzRhcA/B/uXtEST3eShM5uS2sm+oOpxpiRaBv477K2TLpn\r\n"
            "zh82VgwRTDkelpYJG8R8eBXwbBik+WSeIprZ8IrMfzZNs7xwn+z9isB0P3+VObWj\r\n"
            "tBYKdMoF1mu40O1I7hsWV9UpJjRw2WrI8WpZMQTInXcaDWomWRgVhQjVhenYf6u2\r\n"
            "nVoN3MM47hR7OR6KtcQ59lwlBeLcK9ImFuXjPK1GG9LtHzkKVOTI6p51S6Ug5MPu\r\n"
            "7BuJxpxwDiohhVizBk3oZWInpNiXomk7Q6XwQ65mFTB11bX+wn0JIWO7BUWxK+dH\r\n"
            "E+8YuCe53OAvNU0BFT8MAe/vB9kMl4N3p/3bJPuzmP4lnAGrLwzBZPkiQTNq8aj8\r\n"
            "5NsttvIOclBY5A==\r\n"
            "=Zqph\r\n"
            "-----END PGP PUBLIC KEY BLOCK-----\r\n">>,
  [{'RSAPublicKey', _, not_encrypted}] = gpg_key_decode(Key).

-endif.
