%%
%% SNTP - Simple Network Time Protocol (RFC-2030)
%%

-record(sntp, {
    version,    % NTP version (3 or 4)
    stratum,    % 1 - primary ref, 2-15 secondary ref.
    precision,  % Precision of local clock in us.
    rootdelay,  % float() - Total roundtrip delay to the primary reference source in ms.
    rootdisp,   % float() - Nominal error relative to the primary reference source in ms.
    refid,      % Server reference string
    reftime,    % Time at which the local clock was last set (in now() format).
    transtime,  % Time at which the response left NTP server (in now() format).
    delay,      % Roundtrip delay in us.
    offset      % Local clock offset in us.
}).
