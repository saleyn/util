-module(cipher).

-export([encrypt/2, decrypt/2, make_key/1, make_key_base64/1]).
-export([encrypt_base64/2, decrypt_base64/2]).

-define(AAD, <<"AES256GCM">>).

encrypt(Text, KeyPlainText) when (is_list(Text) orelse is_binary(Text)), is_list(KeyPlainText) ->
  Key = make_key(KeyPlainText),
  {encrypt(Text, Key), Key};

encrypt(Text, Key) when (is_list(Text) orelse is_binary(Text)), is_binary(Key), byte_size(Key) =:= 32 ->
  IV = crypto:strong_rand_bytes(16), % create random Initialisation Vector
  {Ciphertext, Tag} = crypto:block_encrypt(aes_gcm, Key, IV, {?AAD, Text, 16}),
  Time = erlang:system_time(microsecond),
  <<IV/binary, Tag/binary, Time:64/integer, Ciphertext/binary>>. % "return" iv with the cipher tag & ciphertext

decrypt(Encrypted, Key) when is_binary(Encrypted), is_binary(Key), byte_size(Key) =:= 32 ->
  <<IV:16/binary, Tag:16/binary, _Time:64/integer, Ciphertext/binary>> = Encrypted,
  crypto:block_decrypt(aes_gcm, Key, IV, {?AAD, Ciphertext, Tag}).

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

-endif.
