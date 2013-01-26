
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision: 95 $
%% @since September 14, 2007

%% @doc <p>ScUtil is StoneCypher's Utility Library, a collection of various routines of a variety of topics:<ul>
%% <li>{@section Conversion}</li>
%% <li>{@section Counters}</li>
%% <li>{@section Dispatch}</li>
%% <li>{@section Documentary}</li>
%% <li>{@section List}</li>
%% <li>{@section Math}</li>
%% <li>{@section Parallelism}</li>
%% <li>{@section Persistence}</li>
%% <li>{@section Random}</li>
%% <li>{@section Regex}</li>
%% <li>{@section Serialism}</li>
%% <li>{@section Statistics}</li>
%% <li>{@section String} and</li>
%% <li>{@section Utility} routines.</li>
%% </ul>
%%         This file has aggregated
%%         dozens of useful miscellaneous routines which I'm releasing to the public in good faith.  There's no particular direction to
%%         this library; any time I write a routine that I tend to use in a lot of situations, which isn't already meaningfully
%%         classifiable into one of my other libraries, I throw it in here.  This has ended up creating a range of unrelated
%%         functionality on which most of my other libraries depend heavily.  Have fun digging around.
%%      </p>
%%
%% == Routines by Category ==
%%
%% The most common way for people to find what they need.
%%
%% <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% === Conversion ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines for converting between basic types, annotated types and user types<br/>
%%     {@link hex_to_int/1}, {@link byte_to_hex/1}, {@link nybble_to_hex/1}, {@link io_list_to_hex/1}, {@link list_to_number/1}
%%   </dd>
%% </dl>
%% === Counters ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines for tracking counters shared between processes<br/>
%%     {@link counter/1}, {@link reset_counter/1}, {@link inc_counter/1}, {@link inc_counter/2}, {@link dec_counter/1}, {@link dec_counter/2}, {@link adjust_counter/2}, {@link set_counter/2}
%%   </dd>
%% </dl>
%% === Dispatch ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines for managing send/receive patterns between processes and process sets<br/>
%%      {@link call_after/2}, {@link call_after/3}, {@link call_after/4}
%%   </dd>
%% </dl>
%% === Documentary ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines whose purpose is to clarify dependant code by presenting their name, rather than routine behavior, as well as to establish standard result messages where appropriate<br/>
%%     {@link even_or_odd/1}, {@link absolute_difference/2}, {@link receive_one/0}, {@link start_register_if_not_running/3}, {@link start_register_if_not_running/4}, {@link start_register_if_not_running/5}
%%   </dd>
%% </dl>
%% === List ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines for operating on lists of data augmenting the standard lists module.<br/>
%%     {@link permute/1}, {@link combinations/1}, {@link shuffle/1}, {@link sanitize_tokens/1}, {@link shared_keys/1}, {@link shared_keys/2}, {@link shared_keys/3}, {@link all_unique_pairings/1}, {@link walk_unique_pairings/2} (see also {@link random_from/3}, {@link random_from_weighted/1})
%%   </dd>
%% </dl>
%% === Math ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines for higher math computation missing from the standard math module.<br/>
%%     {@link list_product/1}, {@link dot_product/2}, {@link cross_product/2}, {@link vector_magnitude/1}, {@link normalize_vector/1}, {@link root_mean_square/1}, {@link root_sum_square/1}, {@link tuple_sum/1}
%%   </dd>
%% </dl>
%% === Parallelism ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines for manipulating behavior across sets of parallel processes.<br/>
%%     {@link map_reduce/2}, {@link map_reduce/3}, {@link map_reduce/4}
%%   </dd>
%% </dl>
%% === Persistence ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines to simplify and automate the storage of local information.<br/>
%%     {@link make_notebook/1}, {@link remove_notebook/1}, {@link has_notebook/1}, {@link annote/3}, {@link read_note/2}, {@link has_note/2}, {@link remove_note/2}
%%   </dd>
%% </dl>
%% === Random ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines to provide complex pseudorandom services and convenience wrappers to augment the standard random module.<br/>
%%     {@link grid_scatter/1}, {@link srand/0}, {@link srand/3}, {@link rand/1}, {@link random_from/1}, {@link random_from/2}, {@link random_from/3}, {@link random_from_weighted/1} (see also {@link shuffle/1})
%%   </dd>
%% </dl>
%% === Regex ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines to provide a convenience wrapper for the standard regex module.<br/>
%%     {@link regex_read_matches/2}, {@link regex_read_matches/3}
%%   </dd>
%% </dl>
%% === Serialism ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines for manipulating behavior in repeat series within a single process.<br/>
%%     {@link multi_do/1}
%%   </dd>
%% </dl>
%% === Statistics ===
%% <dl>
%%   <dt>Means</dt>
%%   <dd>
%%     Routines for finding various kinds of mean values for numeric lists<br/>
%%     {@link arithmetic_mean/1}, {@link geometric_mean/1}, {@link harmonic_mean/1}, {@link weighted_arithmetic_mean/1}, {@link arithmetic_mean/1}
%%   </dd>
%%   <dt>Descriptive</dt>
%%   <dd>
%%     Routines which provide informative measurements of numeric lists<br/>
%%     {@link median/1}, {@link mode/1}, {@link histograph/1}, {@link std_deviation/1}, {@link median_absolute_deviation/1}, {@link moment/1}, {@link moment/2}, {@link central_moment/1}, {@link central_moment/2}, {@link skewness/1}, {@link kurtosis/1} (see also {@link root_mean_square/1})
%%   </dd>
%%   <dt>Normals</dt>
%%   <dd>
%%     Routines to calculate the various normals of vectors<br/>
%%     {@link amean_vector_normal/1}, {@link gmean_vector_normal/1}, {@link hmean_vector_normal/1}
%%   </dd>
%%   <dt>Ranking</dt>
%%   <dd>
%%     Routines to provide statistical rankings of lists<br/>
%%     {@link ranks_of/1}, {@link tied_ranks_of/1}, {@link ordered_ranks_of/1}
%%   </dd>
%%   <dt>Correlations</dt>
%%   <dd>
%%     Routines to measure the statistical correlations between two numeric lists<br/>
%%     {@link kendall_correlation/1}, {@link kendall_correlation/2}, {@link pearson_correlation/1}, {@link pearson_correlation/2}, {@link spearman_correlation/1}, {@link spearman_correlation/2}
%%   </dd>
%% </dl>
%% === String ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines to augment the existing string module.  All scutil string routines are safe for widechar lists.<br/>
%%     {@link sanitize_filename/1}, {@link to_lines/1}
%%   </dd>
%% </dl>
%% === Utility ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines which don't classify well into larger categories<br/>
%%     {@link type_of/1}, {@link get_module_attribute/2}
%%   </dd>
%% </dl>
%%
%% == License ==
%% <p>ScUtil is MIT license, because <a href="http://WhyIHateTheGPL.com/">the author feels very strongly against the GPL</a>.</p>
%%
%% == Thanks ==
%% <p>ScUtil has profited significantly from the donations of time, understanding and code given by a variety of generous friends and strangers.  The list of small tweaks would
%%    be prohibitive, but significant influence on this library is due the following people, in alphabetical order (the least fair of all generic orderings):</p>
%%
%% <ul>
%%   <li>Alain O'Dea of <a href="http://concise-software.blogspot.com/" target="_blank">Concise Software</a></li>
%%   <li>Alisdair Sullivan</li>
%%   <li>Ayrnieu</li>
%%   <li>Bryon Vandiver of <a href="http://sublab.net/" target="_blank">Sublab Research and Design</a></li>
%%   <li>Chile</li>
%%   <li>Dave Murphy / <a href="http://devkitpro.org/" target="_blank">WinterMute</a></li>
%%   <li>DizzyD</li>
%%   <li>Dylan Barrie / PhforSlayer</li>
%%   <li>Etnt</li>
%%   <li>Geoff Cant / <a href="http://github.com/archaelus">Archaelus</a></li>
%%   <li>GrizzlyAdams of <a href="http://grizzly.thewaffleiron.net/" target="_blank">The Waffle Iron</a></li>
%%   <li>Jeff Katz / <a href="http://kraln.com/" target="_blank">Kraln</a></li>
%%   <li>John Sensebe of <a href="http://bargaintuan.com/" target="_blank">Bargaintuan</a></li>
%%   <li>MisterN</li>
%%   <li>raleigh</li>
%%   <li><a href="http://rvirding.blogspot.com/" target="_blank">Robert Virding</a></li>
%%   <li><a href="http://akkit.org/" target="_blank">Steve Stair</a></li>
%%   <li><a href="http://opferman.com/" target="_blank">Toby Opferman</a></li>
%%   <li><a href="http://blueventhorizon.com/" target="_blank">Vat Raghavan</a></li>
%% </ul>
%%
%% @end

%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:20em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:20em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:20em">Direct link to zip archive</span><a href="http://crunchyd.com/release/scutil.zip">Current version</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:20em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:20em">Public SVN at</span><a href="svn://crunchyd.com/scutil/">svn://crunchyd.com/scutil/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:20em">Discussion forum at</span><a href="http://crunchyd.com/forum/scutil-discussion/">CrunchyD Forums</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:20em">Bugtracker at</span><a href="http://crunchyd.com/forum/project.php?projectid=7">CrunchyD Forums</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:20em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date: 2008-12-24 23:52:45 -0500 (Wed, 24 Dec 2008) $</tt></span>
%% @reference <span style="margin-top:1em;padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:20em">Test sets require min. version 16</span><a href="http://testerl.com/">TestErl</a></span>

%% @todo add @see cross-references between related functions
%% @todo add thanks tables and cross-references
%% @todo add dependant libraries table
%% @todo add untested warnings to beginnings of @doc tags
%% @todo add defective warnings to beginnings of @doc tags
%% @todo add links to test data
%% @todo add warnings re: spearman, pearson, kendall use on lists containing repetitions





-module(scutil).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-bugtracker("http://crunchyd.com/forum/project.php?projectid=7").
-publicforum("http://crunchyd.com/forum/scutil-discussion/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id: scutil.erl 95 2008-12-25 04:52:45Z john $").
-svn_head("$HeadURL: svn://crunchyd.com/scutil/scutil.erl $").
-svn_revision("$Revision: 95 $").

-description("StoneCypher's utility library.").

-testerl_export( { [], scutil_testsuite } ).
-library_requirements( [ {testerl,16} ] ).





-export( [

    type_of/1,
    get_module_attribute/2,
    byte_to_hex/1, nybble_to_hex/1, io_list_to_hex/1, % needs tests
    regex_read_matches/2, regex_read_matches/3, regex_read_matches/4, % needs tests
    multi_do/3, multi_do/4, % needs tests
    elements/2, elements/3, elements/4, % needs tests
    sanitize_tokens/2,
    sanitize_filename/1, % needs tests
    random_generator/3, srand/0, srand/3, rand/1, random_from/1, random_from/2, random_from/3, random_from_weighted/1, % needs tests
    grid_scatter/2, % needs tests
    list_product/1, % needs tests
    even_or_odd/1, % needs tests
    histograph/1, % needs tests
    median/1, % needs tests
    mode/1, % needs tests
    arithmetic_mean/1, geometric_mean/1, harmonic_mean/1, weighted_arithmetic_mean/1,  % needs tests
    absolute_difference/2, % needs tests
    std_deviation/1, % needs tests
    root_mean_square/1, root_sum_square/1, % needs tests
    moment/2, moments/1, moments/2, % needs tests
    central_moment/2, central_moments/1, central_moments/2, % needs tests
%    weighted_geometric_mean/1,

    dot_product/2, cross_product/2, % needs tests
    vector_magnitude/1, % needs tests
    qsp_average/2, % needs tests
    normalize_vector/1, % needs tests
    amean_vector_normal/1, gmean_vector_normal/1, hmean_vector_normal/1, % needs tests

    ranks_of/1, % needs tests
    tied_ranks_of/1, % needs tests
    ordered_ranks_of/1, % needs tests
    pearson_correlation/1,  pearson_correlation/2, % needs tests
    spearman_correlation/1, spearman_correlation/2, % needs tests
    kendall_correlation/1,  kendall_correlation/2, % needs tests
    skewness/1, % needs tests
    kurtosis/1, % needs tests
    to_lines/1, % needs tests

    receive_one/0, % needs tests

    shared_keys/1, shared_keys/2, shared_keys/3, % needs tests
    all_unique_pairings/1, % needs tests
    walk_unique_pairings/2, % needs tests
    list_to_number/1, % needs tests
    counter/1, inc_counter/1, inc_counter/2, dec_counter/1, dec_counter/2, reset_counter/1, adjust_counter/2, set_counter/2, counter_process/0, % needs tests
    start_register_if_not_running/3, start_register_if_not_running/4, start_register_if_not_running/5, % needs tests
    wait_until_terminate/0, wait_until_terminate/1, % needs tests
    module_has_function/2, % needs tests

    call_after/2, call_after/3, call_after/4, call_after_worker/4, % needs tests
    shuffle/1, % needs tests

    permute/1, permute/2, % needs tests

    has_bit/2, count_bits/1, % needs tests
    diff_timestamp/2, % needs tests

    expand_label/1, expand_labels/1, % needs tests

    benchmark/3, % needs tests

    hex_to_int/1, % needs tests

    erlang_b_distribution/2, % needs tests
    erlang_c_distribution/2, % needs tests

    implode/2, % needs tests

    mod/2, % needs tests

    scan_svn_revision/1, % needs tests

    median_absolute_deviation/1, % needs tests

    make_notebook/1,    % needs tests
    remove_notebook/1,  % needs tests
    has_notebook/1,     % needs tests
    annote/2, annote/3, % needs tests
    read_note/2,        % needs tests
    has_note/2,         % needs tests
    remove_note/2,      % needs tests

    tuple_sum/1, % needs tests

    map_reduce/2, map_reduce/3, map_reduce/4, % needs tests
    combinations/2 % needs tests

] ).





%% @type typelabel() = [ integer | float | list | tuple | binary | bitstring | boolean | function | pid | port | reference | atom | unknown ].  Used by type_of(), this is just any single item from the list of erlang's primitive types, or the atom <tt>unknown</tt>.

%% @spec type_of(Argument::any()) -> typelabel()

%% @doc {@section Utility} Fetch the type of the argument.  Valid for any term.  Fails before erlang 12, due to use of `is_bitstring()' . ```1> scutil:type_of(1).
%% integer
%%
%% 2> scutil:type_of({hello,world}).
%% tuple'''

%% @since Version 14

type_of(X) when is_integer(X)   -> integer;
type_of(X) when is_float(X)     -> float;
type_of(X) when is_list(X)      -> list;
type_of(X) when is_tuple(X)     -> tuple;
type_of(X) when is_binary(X)    -> binary;
type_of(X) when is_bitstring(X) -> bitstring;  % will fail before erlang 12
type_of(X) when is_boolean(X)   -> boolean;
type_of(X) when is_function(X)  -> function;
type_of(X) when is_pid(X)       -> pid;
type_of(X) when is_port(X)      -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X)      -> atom;

type_of(_X)                     -> unknown.





%% @spec get_module_attribute(Module::atom(), Attribute::atom()) -> { value, {Attribute, Value} } | { error, no_such_attribute } | { error, no_such_module }

%% @doc {@section Utility} <span style="color:red">Buggy</span> Look up an Erlang module attribute value by title.  Originally found at <a href="http://www.astahost.com/info.php/mastering-erlang-part-3-erlang-concurrent_t6632.html">Mastering Erlang Part 3</a>; subsequently cleaned up and given error reporting.  ```1> scutil:get_module_attribute(scutil, author).
%% "John Haugeland <stonecypher@gmail.com>"
%%
%% 2> scutil:get_module_attribute(scutil, license).
%% [{mit_license,"http://scutil.com/license.html"}]'''{@section Thanks} to Alain O'Dea for pointing out defects in this routine regarding repeated module elements, and available improvements to the provided API.  <a href="http://fullof.bs/reading-module-attributes-in-erlang#comment-475" target="_blank">Mr. O'Dea's insightful advice</a> will be implemented, but that time has not yet come.

%% @since Version 23

get_module_attribute(Module,Attribute) ->

    % Found at http://www.astahost.com/info.php/mastering-erlang-part-3-erlang-concurrent_t6632.html
    % Reformatted for clarity, removed unnessecary framing list
    % Added error handling behavior

    case beam_lib:chunks(Module, [attributes]) of

        { ok, { _, [ {attributes,Attributes} ] } } ->
            case lists:keysearch(Attribute, 1, Attributes) of
                { value, {Attribute,Value} }   -> Value;
                false                          -> { error, no_such_attribute }
            end;

        { error, beam_lib, { file_error, _, enoent} } ->
            { error, no_such_module }

    end.





%% @type hexchar() = integer().  Integer must be in the range $0 - $9, the range $a - $f, or the range $A - $F, all inclusive, for inputs; outputs will always use lower case.
%% @type hexstring() = list().  All elements of the list must be of type hexchar() .

%% @spec hex_to_int(HexChar::hexstring() | hexchar()) -> integer()
%% @doc {@section Conversion} Convert a hexstring() or hexchar() into its numeric value. ```1> scutil:hex_to_int("c0ffEE").
%% 12648430
%%
%% 2> scutil:hex_to_int($e).
%% 14
%%
%% 3> scutil:hex_to_int("100").
%% 256'''

%% @since Version 18

hex_to_int(Hex) when is_integer(Hex), Hex >= $0, Hex =< $9 -> Hex - $0;
hex_to_int(Hex) when is_integer(Hex), Hex >= $a, Hex =< $f -> Hex - $a + 10;
hex_to_int(Hex) when is_integer(Hex), Hex >= $A, Hex =< $F -> Hex - $A + 10;

hex_to_int(Hex) when is_list(Hex) -> hex_to_int(Hex, 0).

hex_to_int([],          Acc) -> Acc;
hex_to_int([Digit|Rem], Acc) -> hex_to_int(Rem, (Acc bsl 4) + hex_to_int(Digit)).





%% @type byte() = integer().  A byte must be an integer in the range 0-255, inclusive.  (Technically this is an octet, not a byte, but the word byte is extensively misused throughout the erlang documentation and standard library, which makes this an important concession, so we're when-in-Rome-ing.)

%% @spec byte_to_hex(TheByte::byte()) -> hexstring()

%% @doc {@section Conversion} Convert a byte() into a hexstring().  The hexstring() result will always be two characters (left padded with zero if necessary). ```1> scutil:byte_to_hex(7).
%% "07"
%%
%% 2> scutil:byte_to_hex(255).
%% "ff"'''

%% @since Version 20

byte_to_hex(TheByte) when is_integer(TheByte), TheByte >= 0, TheByte =< 255 -> [ nybble_to_hex(TheByte bsr 4), nybble_to_hex(TheByte band 15) ].





%% @type nybble() = integer().  A nybble must be an integer in the range 0-15, inclusive.

%% @spec nybble_to_hex(Nyb::nybble()) -> integer()

%% @doc {@section Conversion} Convert a nybble() to a hexchar(). ```1> scutil:nybble_to_hex(7).
%% 55
%%
%% 2> scutil:nybble_to_hex(15).
%% 102'''

%% @since Version 19

nybble_to_hex(Nyb) when is_integer(Nyb), Nyb >= 0,  Nyb < 10 -> $0 + Nyb;
nybble_to_hex(Nyb) when is_integer(Nyb), Nyb >= 10, Nyb < 16 -> $a + Nyb - 10.





%% @type io_list() = list().  Every list member of an io_list must be a byte().

%% @spec io_list_to_hex(Input::io_list()) -> hexstring()

%% @doc {@section Conversion} Convert an io_list() to a hexstring().  ```1> scutil:io_list_to_hex("a").
%% "61"
%%
%% 2> scutil:io_list_to_hex("a08n408nbqa").
%% "6130386e3430386e627161"'''

%% @since Version 19

io_list_to_hex(Input) when is_list(Input)                                            -> io_list_to_hex(Input, []).

io_list_to_hex([],               Work)                                               -> lists:reverse(Work);
io_list_to_hex([Item|Remainder], Work) when is_integer(Item), Item >= 0, Item =< 255 -> [A,B] = byte_to_hex(Item), io_list_to_hex(Remainder, [B,A]++Work);
io_list_to_hex(_,                _)                                                  -> {error, not_an_io_list}.





%% @equiv multi_do(C,M,F,[])
%% @since Version 38
multi_do(C, Module, Func)             -> multi_do(C, Module, Func, [],   []).

%% @spec multi_do(Count::integer(), Module::atom(), Function::atom(), Args::list()) -> list()

%% @doc {@section Serialism} Take an iteration count, a module name, a function name and an argument list, and repeatedly apply the argument list to the module/function, count times.  This is primarily useful with nondeterministic functions whose result might change despite identical arguments, such as functions with random behavior; for example, this function is invoked to implement stochastic testing in <a href="http://testerl.com/">TestErl</a>. ```1> scutil:multi_do(10, scutil, rand, [100]).
%% [9,94,4,82,77,44,89,19,45,92]
%%
%% 2> scutil:multi_do(10, scutil, rand, [10000]).
%% [2377,2559,1713,8489,4468,3261,3344,3751,380,2525]'''

%% @since Version 38
multi_do(C, Module, Func, Args)       -> multi_do(C, Module, Func, Args, []).

multi_do(0,_Module,_Func,_Args, Work) -> Work;
multi_do(I, Module, Func, Args, Work) -> multi_do(I-1, Module, Func, Args, Work ++ [apply(Module, Func, Args)]).





%% @equiv regex_read_matches(String, Reg, {0,0})
%% @since Version 41
regex_read_matches(String, Reg) -> regex_read_matches(String, Reg, {0,0}).

%% @equiv regex_read_matches(String, Reg, {TrimFront,TrimLength})
%% @since Version 41
regex_read_matches(String, Reg, TrimFront, TrimLength) -> regex_read_matches(String, Reg, {TrimFront, TrimLength}).

%% @spec regex_read_matches(String::string(), Reg::string(), { TrimFront::integer(), TrimLength::integer() }) -> list() | { error, E }

%% @doc {@section Regex} Take a string and a regular expression (and optionally an offset and length to trim to in each result), and return a list of all matches.  For a trim length of {A,B}, the first A and last B characters of each result will be removed.```1> scutil:regex_read_matches("0j2  4g5  8t9", "[0-9](.)[0-9]").
%% ["0j2","4g5","8t9"]
%%
%% 2> scutil:regex_read_matches("0j2  4g5  8t9", "[0-9](.)[0-9]", {1,1}).
%% ["j","g","t"]
%%
%% 3> scutil:regex_read_matches("0j2  4g5  8t9", "[0-9](.)[0-9]", 1, 1).
%% ["j","g","t"]'''
%%
%% Why provide the equivalent syntaxes (_, _, {A,B}) and (_, _, A,B) ?  Without the tuple is more natural to many, but with the tuple is far more convenient for database-driven behavior, as well as the internal implementation.  I frequently find myself using both forms, and so every time I simplify I find myself wrapping the non-removed form back into the removed form.  Does it violate the simplest interface principle?  Yeah, but in this case it's a boon, IMO.  As such, keeping both forms.

%% @since Version 41
regex_read_matches(String, Reg, {TrimFront, TrimLength}) ->

    case regexp:matches(String, Reg) of
        { match, Matches } -> [ string:substr(String, Start+TrimFront, End-(TrimLength+1)) || {Start,End} <- Matches ];
        { error, E }       -> { error, E }
    end.





%% @type gridsize() = coord() | integer().  Coordinates are the width and height of a (1,1) originated grid; as such, coordinates are of the range [1,X] , [1,Y] inclusive, and returned in the form {A,B}.  The integer form implies a square grid.
%% @type coord() = { number(), number() }.  Represents a coordinate, which may imply a sized rectangle.  Many functions expect integer coordinates; the type does not require them.
%% @type coordlist() = list().  All members of a coordlist() must be coord()s.

%% @spec grid_scatter(Count::integer(), Size::gridsize()) -> coordlist()

%% @doc {@section Random} Return a Count-length list of non-repeating coordinates in a grid of specified size; useful for feature generation.

%% @todo @comeback give code examples (edoc was failing here?)

%% @since Version 42

grid_scatter(0, []) -> []; % skips a lot of work

grid_scatter(Count, {SizeX, SizeY}) -> scutil:random_from(Count, [ {X,Y} || X <- lists:seq(1,SizeX), Y <- lists:seq(1,SizeY) ]);
grid_scatter(Count, Size)           -> grid_scatter(Count, {Size, Size}).





%% @spec srand() -> { ok, { seeded, Seed } }

%% @doc {@section Random} <i style="color:#888">(Called automatically)</i> Instantiates the random source, destroying a prior source if needed, and seeds the source with the clock, returning the seed used.  Generally speaking, you do not need this function; this is used manually when you want to know what seed was used, for purposes of recreating identical pseudorandom sequences.  Otherwise, rand() will call this once on its own.  <em style="color:#a00;font-weight:bold">Because the scutil random system spawns a utility process to maintain random state, this function should be considered to have side effects for purposes of testing.</em> (Indeed, in a sense, this function's entire purpose is to cause a side effect.) ```1> scutil:srand().
%% {ok,{seeded,{1227,902172,685000}}}
%%
%% 2> scutil:srand().
%% {ok,{seeded,{1227,902173,231000}}}'''

%% @since Version 5
%% @todo migrate to labelled random generators, so that concurrent generators do not necessarily interfere with one another

srand() ->

    {A,B,C} = erlang:now(),
    srand(A,B,C).





%% @spec srand(A::integer(), B::integer(), C::integer()) -> { ok, { seeded, Seed } }
%% @doc {@section Random} <i style="color:#888">(Called automatically)</i> Instantiates the random source, destroying a prior source if needed, and seeds the source with the three integer seed you provide, returning the seed used.  Generally speaking, you do not need this function; this is used manually when you want set what seed is used, for purposes of recreating identical pseudorandom sequences.  Otherwise, rand() will call this once on its own.  <em style="color:#a00;font-weight:bold">Because the scutil random system spawns a utility process to maintain random state, this function should be considered to have side effects for purposes of testing.</em> (Indeed, in a sense, this function's entire purpose is to cause a side effect.) ```1> scutil:srand(1,2,3).
%% {ok,{seeded,{1,2,3}}}
%%
%% 2> scutil:srand().
%% {ok,{seeded,{1227,902568,604600}}}
%%
%% 3> scutil:srand(1,2,3).
%% {ok,{seeded,{1,2,3}}}'''

%% @since Version 5
%% @todo migrate to labelled random generators, so that concurrent generators do not necessarily interfere with one another

srand(A,B,C) ->

    RandomGeneratorPid = spawn(?MODULE, random_generator, [A,B,C]),

    case whereis(scutil_rand_source) of
        undefined -> ok;
        _Defined  -> unregister(scutil_rand_source)  % todo fixme leak : this should notify the old rand_source that it is being discarded
    end,

    register(scutil_rand_source, RandomGeneratorPid),
    { ok, { seeded, {A,B,C} } }.





%% @private

random_generator(SeedA, SeedB, SeedC) ->

    random:seed(SeedA, SeedB, SeedC),
    random_generator().





%% @private

random_generator() ->

    receive

        terminate ->
            { ok, terminated };

        [Return, Range] ->
            Val = random:uniform(Range),
            Return ! Val,
            random_generator();

        _  ->
            random_generator()

    end.





%% @spec rand(Range::integer()) -> integer()

%% @doc {@section Random} Returns a pseudorandom integer on the range `[0 - (Range-1)]' inclusive. ```1> scutil:rand(100).
%% 9
%%
%% 2> [ scutil:rand(100) || X <- lists:seq(1,10) ].
%% [12,27,99,86,20,96,28,36,28,15]
%%
%% 3> scutil:histograph([ scutil:rand(10) || X <- lists:seq(1,10000) ]).
%% [{0,992}, {1,990}, {2,992}, {3,1033}, {4,1017}, {5,1003}, {6,996}, {7,1024}, {8,969}, {9,984}]
%%
%% 4> scutil:histograph([ scutil:rand(10) || X <- lists:seq(1,10000) ]).
%% [{0,1028}, {1,979}, {2,934}, {3,970}, {4,1035}, {5,1007}, {6,986}, {7,1012}, {8,1052}, {9,997}]'''

%% @since Version 5

rand(Range) ->

    case whereis(scutil_rand_source) of

        undefined ->
            srand(),
            rand(Range);

        _ ->

            scutil_rand_source ! [ self(), Range ],
            receive RandVal -> RandVal - 1 end

    end.





%% @equiv random_from(1, List, no_remainder)
%% @since Version 6
random_from(   List) -> [X] = random_from(1, List, no_remainder), X.

%% @equiv random_from(N, List, no_remainder)
%% @since Version 6
random_from(N, List) -> random_from(N, List, no_remainder).

%% @spec random_from(N::integer(), List::list(), WantRemainder::want_remainder()) -> list()

%% @doc {@section Random} Take N non-repeating random elements from a list in undefined order.  If the atom `remainder' is passed in as the third argument, the unused portion of the source list will be returned as the second member of a 2ary tuple with the results; the default is no_remainder, which only returns the result set.  Mixed type input lists are perfectly safe, and membership for random selection is shallow (ie, `[ [1,2], [3,4] ]' as an input list would only generate outputs of lists, never integers.)```1> scutil:random_from([monday,tuesday,wednesday,thursday,friday]).
%% friday
%%
%% 2> scutil:random_from(4, lists:seq(1,20)).
%% [6,3,15,12]
%%
%% 3> scutil:random_from(3, [warrior, mage, cleric, thief, paladin, ranger, bard]).
%% [cleric,warrior,ranger]
%%
%% 4> scutil:random_from(6, [mixed, [1,2,3], 4, {five,5}, 3, 67.2, <<"Hello">>, 8]).
%% [[1,2,3],{five,5},4,mixed,<<"Hello">>,67.2]
%%
%% 5> {Team1, Team2} = scutil:random_from(3, [alice,bob,cathy,dave,edward,fawn], remainder).
%% {[cathy,fawn,dave],[bob,edward,alice]}
%%
%% 6> Team1.
%% [cathy,fawn,dave]
%%
%% 7> Where_Food = fun() -> scutil:random_from([deli, fastfood, chinese, mexican, steakhouse, bistro, greek, indian, thai, sushi]) end.
%% #Fun<erl_eval.20.67289768>
%%
%% 8> Where_Food().
%% thai'''

%% @since Version 6
random_from(N, List, no_remainder) -> {R,_} = random_from(N,List,remainder), R;
random_from(N, List, remainder)    -> lists:split(N,shuffle(List)).





%% @type weightedvalue() = { Value::any(), Weight::number() }.  Used by functions like weighted_arithmetic_mean/1 and random_from_weighted/1, weightedvalue()s represent a value with an associated importance or "weight".
%% @type weightlist() = list().  All members of weightlists must be weightedvalue()s.

%% @spec random_from_weighted(InputList::weightlist()) -> any()

%% @doc {@section Random} Take a random single item from a list with weighted probabilities.  Probabilities may be any numeric type, and may be any non-negative value (items with zero probability will be omitted).  Input is a `weightlist()', which is a list in the form `[{Item,Probability}, {I2,P2}, ...]'. There is no requirement to normalize probabilities to any range, though probabilities normalized to ranges will still work as expected. ```1> scutil:random_from([ {quad,4}, {double,2}, {single,1} ]).
%% quad
%%
%% 2> [ scutil:random_from_weighted([ {quad,4}, {double,2}, {single,1} ]) || X <- lists:seq(1,10) ].
%% [single,quad,quad,double,quad,double,quad,quad,quad,double]
%%
%% 3> scutil:histograph([ scutil:random_from_weighted([ {quad,4}, {double,2}, {single,1} ]) || X <- lists:seq(1,777777) ]).
%% [{double,222200},{quad,444165},{single,111412}]'''
%% @since Version 10

% InputList is [ {Item,Weight}, {Item,Weight}, ... ]
random_from_weighted(InputList) when is_list(InputList) ->
    RandomLimit = rand(lists:sum([ Weight || {_,Weight} <- InputList ])),  % the random cap is equal to the sum of all the weights
    random_from_weighted_worker(InputList, RandomLimit).                   % call the worker with the original list and the cap

% if the list is empty, the cap for randomness was calculated wrongly, and as such the random point is too high
random_from_weighted_worker([], _) -> { error, limit_miscalculation };

% but if the list has reasonable contents and the limit is a pos-or-0 integer
random_from_weighted_worker(InputList, Limit) when is_list(InputList), is_integer(Limit), Limit >= 0 ->
    [ {Item,Weight} | Remainder ] = InputList,   % break off the input list's head as {I,W} and keep the rest as Remainder
    case Weight =< Limit of                                             % if the weight is less than or equal to the limit,
        true  -> random_from_weighted_worker(Remainder, Limit-Weight);  % recurse the next item with a decremented weight
        false -> Item                                                   % if not, this item is the one we want
    end.





% todo implement catching tuple { key, reqtype } from list, to auto-convert before return
% todo There may be a crashing bug here for repeated attributes, which are apparently legal, see http://fullof.bs/reading-module-attributes-in-erlang#comment-466
% todo It may help to re-implement this using proplists instead of doing it manually, profile
%% @todo document this

% interface

elements(Config, Requested)                when is_list(Config), is_list(Requested)                     -> elements_worker([], Config, Requested, 1).
elements(Config, Requested, KeyIdx)        when is_list(Config), is_list(Requested), is_integer(KeyIdx) -> elements_worker([], Config, Requested, KeyIdx);

elements(Config, Requested, strip)         when is_list(Config), is_list(Requested)                     -> elements_worker([], Config, Requested, 1,      strip).
elements(Config, Requested, KeyIdx, strip) when is_list(Config), is_list(Requested), is_integer(KeyIdx) -> elements_worker([], Config, Requested, KeyIdx, strip).

% implementation

elements_worker(Retlist, _,      [],        _)      -> Retlist;
elements_worker(Retlist, Config, Requested, KeyIdx) ->

    [ ThisRequest | RemainingRequests ] = Requested,

    case lists:keysearch(ThisRequest, KeyIdx, Config) of
        false            -> elements_worker(Retlist ++ [undefined], Config, RemainingRequests, KeyIdx);
        { value, Tuple } -> elements_worker(Retlist ++ [Tuple],     Config, RemainingRequests, KeyIdx);
        AnythingElse     -> { error, response_not_understood, { for, lists, keysearch, { ThisRequest, Config } }, { got, AnythingElse } }
    end.

elements_worker(Retlist, _,      [],        _,      strip) -> Retlist;
elements_worker(Retlist, Config, Requested, KeyIdx, strip) ->

    [ ThisRequest | RemainingRequests ] = Requested,

    case lists:keysearch(ThisRequest, KeyIdx, Config) of
        false                -> elements_worker(Retlist ++ [undefined], Config, RemainingRequests, KeyIdx, strip);
        { value, {_,Tuple} } -> elements_worker(Retlist ++ [Tuple],     Config, RemainingRequests, KeyIdx, strip);
        AnythingElse         -> { error, response_not_understood, { for, lists, keysearch, { ThisRequest, Config } }, { got, AnythingElse } }
    end.





%% @type filterfunction() = function().  Filter functions are 1ary binary predicates - they accept an argument and return either true or false.
%% @type sanitizer() = list() | filterfunction().  Sanitizers are used by {@link sanitize_tokens/2} for input sanitization; they define what parts of an input list are valid, and the remainder are removed.  Sanitizers may either be a list of acceptable elements or a filter function.

%% @spec sanitize_tokens(InputList::list(), Allowed::sanitizer()) -> list()

%% @doc {@section List} Remove unacceptable elements from an input list, as defined by another list or a filter function.  Common reasons for sanitization include reducing arbitrary or bulk data to key format (such as using an original filename and new size to generate a new filename or database key) and removing malformed items from a list before processing. ```1> scutil:sanitize_tokens("ae0z4nb'wc-04bn ze0e 0;4ci ;e0o5rn;", "ace").
%% "aeceece"
%%
%% 2> Classifier = fun(apple) -> true; (banana) -> true; (cherry) -> true; (date) -> true; (elderberry) -> true; (_) -> false end.
%% #Fun<erl_eval.6.13229925>
%%
%% 3> scutil:sanitize_tokens([apple, boat, cherry, dog, elderberry], Classifier).
%% [apple,cherry,elderberry]'''

%% @see sanitize_filename/1

%% @since Version 31

sanitize_tokens(List, Allowed) when is_list(List), is_function(Allowed) -> lists:filter(Allowed, List);
sanitize_tokens(List, Allowed) when is_list(List), is_list(Allowed)     -> lists:filter(fun(X) -> lists:member(X,Allowed) end, List).





%% @spec sanitize_filename(Filename::string()) -> string()

%% @doc {@section String} Sanitize an arbitrary string to be appropriate for Windows and Unix filesystems, and URLs. ```1> scutil:sanitize_filename("\h/e~l%lo! w^o@r#l*d.").
%% "helloworld"'''

%% @see sanitize_tokens/2

%% @since Version 31

sanitize_filename(Filename) -> sanitize_tokens(Filename, lists:seq($a,$z)++lists:seq($A,$Z)++lists:seq($0,$9)++"-_()[]").





%% @todo finish me

% batch_reduce(Workload, Function) ->
%
%     [ spawn(Function, LoadItem) || LoadItem <- Workload ],
%     reduce_receive(length(Workload, [])).
%
% reduce_receive(0,           Work) -> Work;
% reduce_receive(AnswerCount, Work) -> receive X -> reduce_receive(AnswerCount-1, [X]++Work).





%% @todo finish me

% distributed_batch_reduce(Workload, Function, Nodes) -> distributed_batch_reduce_handout(Workload, Function, Nodes, [], 0).

% distributed_batch_reduce_handout([],            _Function, _Nodes,              Work, 0)        -> Work;                                                                                                                                   % nothing left in the work queue, count of output outstanding is 0, work's done
% distributed_batch_reduce_handout([],             Function, _Nodes,              Work, CountOut) -> { item, {_Source, Result } } = scutil:receive_one(), distributed_batch_reduce_handout([],       Function,  [],       [Result]++Work, CountOut-1); % there's no work left in the queue, but stuff outstanding from child nodes
% distributed_batch_reduce_handout(Workload,       Function,  [],                 Work, CountOut) -> { item, { Source, Result } } = scutil:receive_one(), distributed_batch_reduce_handout(Workload, Function,  [Source], [Result]++Work, CountOut-1); % no nodes available, wait for a receive, queue the result and add the node back to the available list
% distributed_batch_reduce_handout([Item|WorkRem], {Mod,Fun}, [ThisNode|NodeRem], Work, CountOut) -> spawn(ThisNode, Mod, Fun, Item),           distributed_batch_reduce_handout(WorkRem,  {Mod,Fun}, NodeRem,  Work,           CountOut+1). % work and nodes available; dispatch some work, increment the work out counter and recurse





%% @todo finish me

% dissimilar_charset(english, lowercase) -> "abcdefghjklmnopqrstuwxyz";
% dissimilar_charset(english, mixedcase) -> "abcdefghjklmnopqrstuwxyzABDEFGHRT";
% dissimilar_charset(english, alphanum)  -> "abcdefghjklmnopqrstuwxyzABDEFGHRT34679".

% similarize_charset   a10OZ2B8 -> aloozzBB





%% @spec receive_one() -> { item, any() } | nothing_there

%% @doc {@section Utility} Pop the front of the message queue and return it as `{item,X}', or return nothing_there for empty queues; do not block.  ```1> scutil:receive_one().
%% nothing_there
%%
%% 2> self() ! message.
%% message
%%
%% 3> scutil:receive_one().
%% {item,message}
%%
%% 4> scutil:receive_one().
%% nothing_there'''

%% @since Version 2

receive_one() ->

    receive (X) -> { item, X }
    after 0     -> nothing_there
    end.





%% @type numericlist() = list().  All members of a numeric list must be number()s.
%% @spec arithmetic_mean(InputList::numericlist()) -> float()

%% @doc {@section Statistics} Take the arithmetic mean (often called the average) of a list of numbers. ```1> scutil:arithmetic_mean([1,2,3,4,5]).
%% 3.0'''

%% @see geometric_mean/1
%% @see harmonic_mean/1
%% @see weighted_arithmetic_mean/1
%% @see amean_vector_normal/1

%% @since Version 33

arithmetic_mean([])                      -> 0.0;
arithmetic_mean(List) when is_list(List) -> lists:sum(List) / length(List).





%% @spec geometric_mean(InputList::numericlist()) -> float()

%% @doc {@section Statistics} Take the geometric mean of a list of numbers. ```1> scutil:geometric_mean([1,2,3,4,5]).
%% 2.6051710846973517''' The naive approach ```geometric_mean(List) -> math:pow(scutil:list_product(List), 1/length(List)).''' is not used because it accumulates error very quickly, and is as such unsuited to huge lists.

%% @see arithmetic_mean/1
%% @see harmonic_mean/1
%% @see gmean_vector_normal/1

%% @since Version 34

geometric_mean([])                       -> 0.0;
geometric_mean(List)  when is_list(List) -> math:exp(scutil:arithmetic_mean([math:log(X)||X<-List])).





%% @spec harmonic_mean(InputList::numericlist()) -> float()

%% @doc {@section Statistics} Take the harmonic mean of a list of numbers. ```1> scutil:harmonic_mean([1,2,3,4,5]).
%% 2.18978102189781'''

%% @see arithmetic_mean/1
%% @see geometric_mean/1
%% @see hmean_vector_normal/1

%% @since Version 35

harmonic_mean([])                        -> 0.0;
harmonic_mean(List)   when is_list(List) -> length(List) / lists:sum([ 1/X || X<-List ]).





%% @spec weighted_arithmetic_mean(InputList::weightlist()) -> float()

%% @doc {@section Statistics} Take the weighted arithmetic mean of the input values. ```1> scutil:weighted_arithmetic_mean([ {8,1}, {3,4}, {16,1} ]).
%% 6.0'''

%% @see arithmetic_mean/1
%% @see amean_vector_normal/1

%% @since Version 44

weighted_arithmetic_mean(List)   when is_list(List) -> weighted_arithmetic_mean(List, 0, 0).

weighted_arithmetic_mean([],           Num, Denom)  -> Num/Denom;
weighted_arithmetic_mean([{V,W}|Tail], Num, Denom)  -> weighted_arithmetic_mean(Tail, Num+(W*V), Denom+W).





%% @spec even_or_odd(Num::integer()) -> even | odd

%% @doc {@section Documentary} Documentary convenience function that returns the atoms `even' or `odd' for any integer. ```1> scutil:even_or_odd(3).
%% odd'''

%% @since Version 8

even_or_odd(Num) when is_integer(Num), Num band 1 == 0 -> even;
even_or_odd(Num) when is_integer(Num)                  -> odd.





%% @spec median(List::numericlist()) -> any()

%% @doc {@section Statistics} Takes the median (central) value of a list.  Sorts the input list, then finds and returns the middle value.  ```1> scutil:median([1,2,999]).
%% 2'''

%% @see arithmetic_mean/1
%% @see mode/1

%% @since Version 8

median(List) when is_list(List) ->

    SList = lists:sort(List),
    Length = length(SList),
    case even_or_odd(Length) of
        even -> [A,B] = lists:sublist(SList, round(Length/2), 2), (A+B)/2;
        odd  -> lists:nth( round((Length+1)/2), SList )
    end.





%% @spec mode(List::numericlist()) -> any()

%% @doc {@section Statistics} Takes the mode (most common) value(s) of a list, as a list.  If there are more than one value tied for most common, all tied will be returned.  This function is safe for mixed-type lists, and does not perform deep traversal (that is, the mode of `[ [2,2] ]' is `[2,2]', not `2'). ```scutil:mode([1,2,1,3,1,4]).
%% [1]
%%
%% 2> scutil:mode([ [1,2,3], [2,3,4], [3,4,5], [2,3,4] ]).
%% [[2,3,4]]
%%
%% 3> scutil:mode([ a,b, 1, a,b, 2, a,b, 3 ]).
%% [a,b]'''

%% @see arithmetic_mean/1
%% @see median/1

%% @since Version 8

mode([])                      -> [];
mode(List) when is_list(List) -> mode_front(lists:reverse(lists:keysort(2, scutil:histograph(List)))).

mode_front([{Item,Freq}|Tail])                      -> mode_front(Tail, Freq, [Item]).

mode_front([ {Item, Freq} | Tail], Freq,   Results) -> mode_front(Tail, Freq, [Item]++Results);
mode_front([{_Item,_Freq} |_Tail],_Better, Results) -> Results;
mode_front([],                    _Freq,   Results) -> Results.





%% @spec absolute_difference(A::number(), B::number()) -> number()

%% @doc {@section Documentary} Takes the absolute value of the difference between the two arguments.  Offered mostly to make dependant code clearer. ```1> scutil:absolute_difference(1.25, 1).
%% 0.25'''

%% @since Version 39

absolute_difference(A,B) -> abs(A-B).





%% @spec list_product(A::numericlist()) -> number()

%% @doc {@section Math} Takes the product of all numbers in the list.  Offered mostly to make dependant code clearer. ```1> scutil:list_product([1,2,5.4]).
%% 10.8'''

%% @since Version 39

list_product(List) when is_list(List) -> list_product(List, 1).

list_product([],          Counter) -> Counter;
list_product([Head|Tail], Counter) -> list_product(Tail, Counter*Head).





%% @spec histograph(List::list()) -> weightlist()

%% @doc {@section Statistics} Takes a histograph count of the items in the list.  Mixed type lists are safe.  Input lists do not need to be sorted.  The histograph is shallow - that is, the histograph of `[ [1,2], [1,2], [2,2] ]' is `[ {[1,2],2}, {[2,2],1} ]', not `[ {1,2}, {2,4} ]'. ```1> scutil:histograph([1,2,a,2,b,1,b,1,b,2,a,2,2,1]).
%% [{1,4},{2,5},{a,2},{b,3}]
%%
%% 2> scutil:histograph([ scutil:rand(10) || X <- lists:seq(1,100000) ]).
%% [{0,10044}, {1,9892}, {2,10009}, {3,10016}, {4,10050}, {5,10113}, {6,9990}, {7,9994}, {8,10004}, {9,9888}]'''

%% @since Version 19

%% @todo add an argument presort to this and other functions to skip the sorting pass

histograph(List) when is_list(List) ->

    [Head|Tail] = lists:sort(List),
    histo_count(Tail, Head, 1, []).

histo_count([],             Current, Count, Work) -> lists:reverse([{Current,Count}]++Work);
histo_count([Current|Tail], Current, Count, Work) -> histo_count(Tail, Current, Count+1, Work);
histo_count([New|Tail],     Current, Count, Work) -> histo_count(Tail, New,     1,       [{Current,Count}]++Work).





%% @spec std_deviation(Values::numericlist()) -> float()

%% @doc {@section Statistics} Measures the standard deviation of the values in the list.  ```1> scutil:std_deviation([1,2,3,4,5]).
%% 1.4142135623730951
%%
%% 2> scutil:std_deviation([2,2,2,2,2]).
%% 0.0'''

%% @since Version 39

std_deviation(Values) when is_list(Values) ->

    Mean = arithmetic_mean(Values),
    math:sqrt(arithmetic_mean([ (Val-Mean)*(Val-Mean) || Val <- Values ])).





%% @spec root_mean_square(Values::numericlist()) -> float()

%% @doc {@section Statistics} Calculates the root mean square of the values in the list.  ```1> scutil:root_mean_square([1,2,3,4,5]).
%% 3.3166247903554
%%
%% 2> scutil:root_mean_square([2,2,2]).
%% 2.0'''

%% @since Version 39

root_mean_square(List) when is_list(List) -> math:sqrt(arithmetic_mean([ Val*Val || Val <- List ])).





%% @type ranking() = { Ranking::number(), Value::any() }.  Values are usually number()s, but do not have to be with custom ranking predicates.
%% @type rankinglist() = list().  Members of a rankinglist() must all be ranking()s.

%% @todo comeback make a ranks_of/2 which takes a sorting predicate
%% @spec ranks_of(Values::numericlist()) -> rankinglist()

%% @doc {@section Statistics} Returns a ranked ordering of the list without tie rankings.  ```1> scutil:ranks_of([10,90,20,80,30,70,40,60,50]).
%% [{1,90}, {2,80}, {3,70}, {4,60}, {5,50}, {6,40}, {7,30}, {8,20}, {9,10}]
%%
%% 2> scutil:ranks_of([10,10,10,10]).
%% [{1,10},{2,10},{3,10},{4,10}]'''

%% @since Version 42

ranks_of(List) when is_list(List) -> lists:zip(lists:seq(1,length(List)),lists:reverse(lists:sort(List))).





%% @todo comeback make a tied_ranks_of/2 which takes a sorting predicate
% needs significant refactoring; work is being repeated

%% @spec tied_ranks_of(Values::numericlist()) -> rankinglist()

%% @doc {@section Statistics} Returns a ranked ordering of the list with tie rankings.  As such, for uniformity, all rankings are floats.  Ties are represented as the centers of ranges. ```1> scutil:tied_ranks_of([10,90,20,80,30,70,40,60,50]).
%% [{1.0,90}, {2.0,80}, {3.0,70}, {4.0,60}, {5.0,50}, {6.0,40}, {7.0,30}, {8.0,20}, {9.0,10}]
%%
%% 2> scutil:tied_ranks_of([100,200,200,300]).
%% [{1.0,300},{2.5,200},{2.5,200},{4.0,100}]'''

%% @since Version 42

tied_ranks_of(List) -> tied_rank_worker(ranks_of(List), [], no_prev_value).

tied_add_prev(Work, {FoundAt, NewValue}) -> lists:duplicate(length(FoundAt),{lists:sum(FoundAt) / length(FoundAt), NewValue}) ++ Work.


tied_rank_worker([],               Work, PrevValue) -> lists:reverse(tied_add_prev(Work, PrevValue));
tied_rank_worker([Item|Remainder], Work, PrevValue) ->
    case PrevValue of
        no_prev_value ->
            {BaseRank,BaseVal} = Item,
            tied_rank_worker(Remainder, Work, {[BaseRank],BaseVal});
        {FoundAt,OldVal} ->
            case Item of
                {Id,OldVal} ->
                    tied_rank_worker(Remainder, Work,                           {[Id]++FoundAt,OldVal});
                {Id,NewVal} ->
                    tied_rank_worker(Remainder, tied_add_prev(Work, PrevValue), {[Id],NewVal})
            end
    end.





%% @todo comeback make a tied_ranks_of/2 which takes a sorting predicate

%% @spec ordered_ranks_of(Values::numericlist()) -> rankinglist()

%% @doc {@section Statistics} Returns a tied ranked ordering of the list, ordered according to the input ordering rather than the sorted ordering.  As with {@link tied_ranks_of/1}, all rankings are floats, and ties are represented as the centers of ranges. ```1> scutil:ordered_ranks_of([10,90,20,80,30,70,40,60,50]).
%% [{9.0,10}, {1.0,90}, {8.0,20}, {2.0,80}, {7.0,30}, {3.0,70}, {6.0,40}, {4.0,60}, {5.0,50}]
%%
%% 2> scutil:ordered_ranks_of([100,200,200,300]).
%% [{4.0,100},{2.5,200},{2.5,200},{1.0,300}]'''

%% @since Version 42

ordered_ranks_of(List) when is_list(List) ->
    ordered_ranks_of(List, tied_ranks_of(List), []).

ordered_ranks_of([], [], Work) -> lists:reverse(Work);

ordered_ranks_of([Front|Rem], Ranks, Work) ->
    {value,Item} = lists:keysearch(Front,2,Ranks),
    {IRank,Front} = Item,
    ordered_ranks_of(Rem, Ranks--[Item], [{IRank,Front}]++Work).





%% @type stringlist() = list().  Every member of a stringlist() is a string().

%% @spec to_lines(Text::string()) -> stringlist()

%% @doc {@section String} Cuts a string according to any of the three newline conventions (even mixed), and discards empty strings. ```1> scutil:to_lines("one\rtwo\nthree\r\nfour\r\r\rfive").
%% ["one","two","three","four","five"]'''

%% @since Version 2

to_lines(Text) -> string:tokens(Text, "\r\n"). % yay convenience functions





%% @todo use test data at http://changingminds.org/explanations/research/analysis/pearson.htm

%% @spec pearson_correlation(TupleList::coordlist()) -> { r, Correlation::number() }

%% @doc {@section Statistics} Compute the Pearson Correlation Coefficient of a list of coordinate tuples. ```1> scutil:pearson_correlation([ {1,1}, {2,2}, {3,3}, {4,4}, {5,5} ]).
%% {r,1.0}
%%
%% 2> scutil:pearson_correlation([ {1,5}, {2,4}, {3,3}, {4,2}, {5,1} ]).
%% {r,-1.0}
%%
%% 3> scutil:pearson_correlation([ {1,3}, {2,3}, {3,3}, {4,3}, {5,3} ]).
%% {r,0.0}
%%
%% 4> scutil:pearson_correlation([ {1,2}, {2,2.5}, {3,3}, {4,3.5}, {5,4} ]).
%% {r,1.0}
%%
%% 5> scutil:pearson_correlation([ {1,2}, {2,2.4}, {3,3}, {4,3.6}, {5,4} ]).
%% {r,0.9970544855015818}'''
%%
%% @since Version 49

pearson_correlation(TupleList) when is_list(TupleList) ->
    {A,B} = lists:unzip(TupleList),
    pearson_correlation(A,B).

%% @equiv pearson_correlation(lists:zip(List1, List2))

pearson_correlation(List1, _)     when length(List1) < 2 -> {r,0.0};

pearson_correlation(List1, List2) when is_list(List1), is_list(List2), length(List1) /= length(List2) -> {error, lists_must_be_same_length};
pearson_correlation(List1, List2) when is_list(List1), is_list(List2) ->

    SumXY = lists:sum([A*B || {A,B} <- lists:zip(List1,List2) ]),   % the sum of the products of each matched pair

    SumX  = lists:sum(List1),
    SumY  = lists:sum(List2),

    SumXX = lists:sum([L*L || L<-List1]),                           % the sums of the squared items
    SumYY = lists:sum([L*L || L<-List2]),

    N     = length(List1),

    case math:sqrt(   ( (N*SumXX)-(SumX*SumX) )   *   ( (N*SumYY)-(SumY*SumY) )   ) of
        0     -> {r,0.0};  % some nasty value sets otherwise cause divide by zero
        0.0   -> {r,0.0};  % eg [ [1,1,1,1,1], [1,1,2,1,2] ]
        Denom ->
          Numer = (N*SumXY) - (SumX * SumY),
          {r, (Numer/Denom)}
    end.





%% @todo use test data at http://geographyfieldwork.com/SpearmansRank.htm

%% @spec spearman_correlation(TupleList::coordlist()) -> { rsquared, Correlation::number() }

%% @doc {@section Statistics} Compute the Spearman's Rank Correlation Coefficient of a list of coordinate tuples. ```1> scutil:spearman_correlation([ {1,1}, {2,2}, {3,3}, {4,4}, {5,5} ]).
%% {rsquared,1.0}
%%
%% 2> scutil:spearman_correlation([ {1,5}, {2,4}, {3,3}, {4,2}, {5,1} ]).
%% {rsquared,-1.0}
%%
%% 3> scutil:spearman_correlation([ {1,3}, {2,3}, {3,3}, {4,3}, {5,3} ]).
%% {rsquared,0.5}
%%
%% 4> scutil:spearman_correlation([ {1,2}, {2,2.5}, {3,3}, {4,3.5}, {5,4} ]).
%% {rsquared,1.0}
%%
%% 5> scutil:spearman_correlation([ {1,2}, {2,2.4}, {3,3}, {4,3.6}, {5,4} ]).
%% {rsquared,1.0}'''

%% @since Version 50

spearman_correlation(TupleList) when is_list(TupleList) ->
    {A,B} = lists:unzip(TupleList),
    spearman_correlation(A,B).

%% @equiv spearman_correlation(lists:zip(List1, List2))

spearman_correlation(List1, _)     when length(List1) < 2 -> {rsquared,0.0};

spearman_correlation(List1, List2) when is_list(List1), is_list(List2), length(List1) /= length(List2) -> {error, lists_must_be_same_length};
spearman_correlation(List1, List2) when is_list(List1), is_list(List2) ->

    {TR1,_} = lists:unzip(ordered_ranks_of(List1)),
    {TR2,_} = lists:unzip(ordered_ranks_of(List2)),

    Numerator   = 6 * lists:sum([ (D1-D2)*(D1-D2) || {D1,D2} <- lists:zip(TR1,TR2) ]),
    Denominator = math:pow(length(List1),3)-length(List1),

    {rsquared,1-(Numerator/Denominator)}.





%% @todo use test data at http://changingminds.org/explanations/research/analysis/kendall.htm

%% @spec kendall_correlation(TupleList::coordlist()) -> { tau, Correlation::number() }

%% @doc {@section Statistics} Compute the Kendall Tau Rank Correlation Coefficient of a list of coordinate tuples. ```1> scutil:kendall_correlation([ {1,1}, {2,2}, {3,3}, {4,4}, {5,5} ]).
%% {tau,1.0}
%%
%% 2> scutil:kendall_correlation([ {1,5}, {2,4}, {3,3}, {4,2}, {5,1} ]).
%% {tau,-1.0}
%%
%% 3> scutil:kendall_correlation([ {1,3}, {2,3}, {3,3}, {4,3}, {5,3} ]).
%% {tau,1.0}
%%
%% 4> scutil:kendall_correlation([ {1,2}, {2,2.5}, {3,3}, {4,3.5}, {5,4} ]).
%% {tau,1.0}
%%
%% 5> scutil:kendall_correlation([ {1,2}, {2,2.4}, {3,3}, {4,3.6}, {5,4} ]).
%% {tau,1.0}'''

%% @since Version 51

kendall_correlation(TupleList) when is_list(TupleList) ->
    {A,B} = lists:unzip(TupleList),
    kendall_correlation(A,B).

%% @equiv kendall_correlation(lists:zip(List1, List2))

kendall_correlation(List1, _)     when length(List1) < 2 -> {tau,0.0};

kendall_correlation(List1, List2) when is_list(List1), is_list(List2), length(List1) /= length(List2) -> {error, lists_must_be_same_length};
kendall_correlation(List1, List2) when is_list(List1), is_list(List2) ->

    {RA,_} = lists:unzip(ordered_ranks_of(List1)),
    {RB,_} = lists:unzip(ordered_ranks_of(List2)),

    Ordering = lists:keysort(1,lists:zip(RA,RB)),
    {_,OrdB} = lists:unzip(Ordering),

    N = length(List1),
    P = lists:sum(kendall_right_of(OrdB, [])),

    {tau, -(( (4*P) / (N * (N - 1))) - 1) }.

kendall_right_of([],    Work) -> lists:reverse(Work);
kendall_right_of([F|R], Work) -> kendall_right_of(R, [kendall_right_of_item(F,R)]++Work).

kendall_right_of_item(B, Rem) -> length([R || R <- Rem, R < B]).





% thanks to Chile and Kraln for straightening me out on moments and central moments

%% @spec moment(List::list(), N::number()) -> float()

%% @doc {@section Statistics} Takes the Nth moment of a list.  The Nth moment of a list is the arithmetic mean of the list items, each taken to the Nth power.  Fractional Ns are well defined
%% and have obscure uses, though most will only ever use this with integer values of N; this function is valid for both.  Not to be confused with {@link central_moment/2}.  {@section Thanks}
%% to Kraln and Chile for straightening me out on moments and central moments.  ```1> scutil:moment([1,1,1], 2).
%% 1.0
%%
%% 2> scutil:moment([2,2,2], 2).
%% 4.0
%%
%% 3> scutil:moment([1,2,3], 2).
%% 4.666666666666667
%%
%% 4> scutil:moment([1,2,3], 3).
%% 12.0
%%
%% 5> scutil:moment([1,2,3], 3.5).
%% 19.693026767781483'''

%% @since Version 50

moment(List, N) when is_list(List), is_number(N) ->
    scutil:arithmetic_mean( [ math:pow(Item, N) || Item <- List ] ).

%% @equiv [ moment(List, N) || N <- [2,3,4] ]

moments(List)                                -> moments(List, [2,3,4]).

%% @equiv [ moment(List, N) || N <- Moments ]

moments(List, Moments) when is_list(Moments) -> [ moment(List, M) || M <- Moments ].





% thanks to Chile and Kraln for straightening me out on moments and central moments

%% @spec central_moment(List::list(), N::integer()) -> float()

%% @doc {@section Statistics} Takes the Nth cetral moment of a list.  The Nth central moment of a list is the arithmetic mean of (the list items each minus the mean of the list, each 
%% taken to the Nth power).  In a sense, this is the "normalized" moment.  Fractional Ns are not defined.  Not to be confused with {@link moment/2}.  {@section Thanks} to Kraln and
%% Chile for straightening me out on moments and central moments.  ```1> scutil:central_moment([1,1,1], 2).
%% 0.0
%%
%% 2> scutil:central_moment([2,2,2], 2).
%% 0.0
%%
%% 3> scutil:central_moment([1,2,3], 2).
%% 0.666666666666666
%%
%% 4> scutil:central_moment([1,2,3], 3).
%% 0.0'''

%% @since Version 50

central_moment(List, N) when is_list(List), is_integer(N) ->
    ListAMean = scutil:arithmetic_mean(List),
    scutil:arithmetic_mean( [ math:pow(Item-ListAMean, N) || Item <- List ] ).

%% @equiv [ central_moment(List, N) || N <- [2,3,4] ]
central_moments(List)                                -> central_moments(List, [2,3,4]).

%% @equiv [ central_moment(List, N) || N <- Moments ]
central_moments(List, Moments) when is_list(Moments) -> [ central_moment(List, M) || M <- Moments ].





%% @equiv central_moment(List, 3)
skewness(List) -> central_moment(List, 3).

%% @equiv central_moment(List, 4)
kurtosis(List) -> central_moment(List, 4).





% quadratic scalar product average
% see http://www.inf.fu-berlin.de/inst/ag-ki/rojas_home/documents/1996/NeuralNetworks/K5.pdf pdf-page 15
% Thanks to the following for help with qsp_average and dependencies: Asterick, Chile, John Sensebe, PfhorSlayer, Raleigh

%% @type vector() = list() | tuple().  Every member element of a vector() is a {@type number()}.
%% @type vectorlist() = list().  Every member element of a vectorlist() is a {@type vector()}.

%% @spec qsp_average(W::numericlist(), InputVecs::vectorlist()) -> float()

%% @doc {@section Math} Takes the quadratic scalar product average of a vector `W' and a list of vectors `X'.  The QSP Average
%% is the arithmetic mean of the result set Y, where Y is generated as the square of the magnitude of the dot product
%% of W and each individual vector in X.```1> scutil:qsp_average([1,2,3], [[0,0,0],[0,0,0]]).
%% 0.0
%%
%% 2> scutil:qsp_average([1,2,3], [[0,0,1],[0,0,0]]).
%% 4.5
%%
%% 3> scutil:qsp_average([1,2,3], [[0,1,0],[0,0,0]]).
%% 2.0
%%
%% 4> scutil:qsp_average([1,2,3], [[1,0,0],[0,0,0]]).
%% 0.5
%%
%% 5> scutil:qsp_average([1,2,3], [[1,1,1],[0,0,0]]).
%% 18.0
%%
%% 6> scutil:qsp_average([1,2,3], [[0,0,0],[1,1,1]]).
%% 18.0
%%
%% 7> scutil:qsp_average([1,2,3], [[1,1,1],[1,1,1]]).
%% 36.0'''The linked documentation incorrectly uses the notation ||Foo|| instead of |Foo| to
%% present the algorithm.  ||Foo|| is the vector magnitude - the root sum square of vector elements - but as the input is the
%% dot product of two 1d vectors, which will always be a single number, the vector magnitude serves no purpose other than to
%% normalize the sign slowly and counterintuitively; thus we switch to abs despite the documentation.  {@section Thanks} to Steve
%% Stair for helping straighten this out.

%% @since Version 82

qsp_average(W, InputVecs) ->

    GetSqVnDp = fun(Xi) ->
        VnDp = abs(dot_product(W, Xi)),
        VnDp * VnDp
        end,

    arithmetic_mean([ GetSqVnDp(Xi) || Xi <- InputVecs ]).





% removed when length(VX) == length(VY) because it's implied by lists:zip

%% @spec dot_product(VX::numeric_list(), VY::numeric_list()) -> number()

%% @doc {@section Math} <span style="color:red">Incomplete</span> Calculates the dot product of two vectors (<span style="color:red">Incomplete</span> represented as numeric lists; tuples not yet supported). ```1> scutil:dot_product([1,1,1],[2,2,2]).
%% 6
%%
%% 2> scutil:dot_product([1,1,1],[3,3,3]).
%% 9
%%
%% 3> scutil:dot_product([-1,0,1],[3,3,3]).
%% 0
%%
%% 4> scutil:dot_product([-1,1,1],[3,3,3]).
%% 3
%%
%% 5> scutil:dot_product([0.5,1,2],[1,1,1]).
%% 3.5'''<span style="color:red">TODO: The tuple variation of vectors has not yet been implemented in this function.</span>

%% @since Version 80

%% @todo implement tuple variation

dot_product(VX, VY) ->
    lists:sum( [ X*Y || {X,Y} <- lists:zip(VX,VY) ] ).





%% @type three_vector() = vector().  A three-vector always has three elements, so this can be expressed as the alternation `{A::number(), B::number(), C::number()} | [A::number(), B::number(), C::number()]'.
%% @type seven_vector() = vector().  A seven-vector always has seven elements, so this can be expressed as the alternation `{A::number(), B::number(), C::number(), D::number(), E::number(), F::number(), G::number()} | [A::number(), B::number(), C::number(), D::number(), E::number(), F::number(), G::number()]'.
%% @type three_or_seven_vector() = three_vector() | seven_vector().

%% @spec cross_product(VX::three_vector(), VY::three_vector()) -> three_vector()

%% @doc {@section Math} <span style="color:red">Incomplete</span> Calculates the cross product of two vectors (<span style="color:red">Incomplete</span> represented as {@type three_vector()}s - no support yet for seven). ```1> scutil:dot_product([1,1,1],[2,2,2]).
%% 6
%%
%% 2> scutil:dot_product([1,1,1],[3,3,3]).
%% 9
%%
%% 3> scutil:dot_product([-1,0,1],[3,3,3]).
%% 0
%%
%% 4> scutil:dot_product([-1,1,1],[3,3,3]).
%% 3
%%
%% 5> scutil:dot_product([0.5,1,2],[1,1,1]).
%% 3.5'''<span style="color:red">TODO: Implement seven-dimensional cross product</span>

%% @since Version 80

%% @todo implement 7-dimensional variation, http://en.wikipedia.org/wiki/Seven-dimensional_cross_product

cross_product( {X1,Y1,Z1}, {X2,Y2,Z2} ) ->
    { (Y1*Z2) - (Z1*Y2) , (Z1*X2) - (X1*Z2), (X1*Y2) - (Y1*X2) };

cross_product( [X1,Y1,Z1], [X2,Y2,Z2] ) ->
    [ (Y1*Z2) - (Z1*Y2) , (Z1*X2) - (X1*Z2), (X1*Y2) - (Y1*X2) ].





%% @type numeric_tuple() = tuple().  Every member of a {@type numeric_tuple()} must be a {@type number()}.
%% @type relaxed_numeric_tuple() = numeric_tuple().  Relaxed numeric tuples are allowed to contain non-numeric elements, which are treated as zero for purposes of computation.

%% @spec tuple_sum(T::relaxed_numeric_tuple()) -> number()

%% @doc {@section Math} Returns the sum of the numeric elements of a tuple, treating non-numeric elements as zero. ```1>'''

%% @since Version 86

tuple_sum(T) when is_tuple(T) -> tuple_sum(T, 1, size(T), 0).

tuple_sum(_T, Which, Max, Work) when Which > Max -> Work;
tuple_sum( T, Which, Max, Work)                  -> tuple_sum(T, Which+1, Max, Work+element(Which, T)).







%% @spec root_sum_square(VX::vector()) -> number()

%% @doc {@section Math} Calculate the magnitude (also known as the root sum square)

%% @since Version 85

root_sum_square(VX) when is_list(VX) ->
    math:sqrt(lists:sum([ X*X || X <- VX ]));

root_sum_square(VX) when is_tuple(VX) -> root_sum_square(tuple_to_list(VX)).

%% @equiv root_sum_square(VX)

%% @doc {@section Math} Returns the magnitude of a vector.  A vector's magnitude is the length of its hypoteneuse (and is as such the root sum square of its components).  A vector can be seen as the product of its unit vector and its magnitude; as such many people see a vector's magnitude as its scale. ```1> scutil:vector_magnitude([0,0,0]).
%% 0.0
%%
%% 2> scutil:vector_magnitude([1,0,0]).
%% 1.0
%%
%% 3> scutil:vector_magnitude([1,1,1]).
%% 1.7320508075688772
%%
%% 4> scutil:vector_magnitude([1,2,3]).
%% 3.7416573867739413
%%
%% 5> scutil:vector_magnitude([0,0.4,0.6,0.2,0.4,0.529150262213]).
%% 1.0000000000000433'''

%% @since Version 85

vector_magnitude(VX) -> root_sum_square(VX).





%% @type unit_vector() = vector().  The hypoteneuse of a unit vector is precisely one unit long.  Unit vectors are also called normalized or magnitude-normalized vectors.

%% @spec normalize_vector(Vector::vector()) -> unit_vector()

%% @doc {@section Math} Returns the magnitude of a vector.  A vector's magnitude is the length of its hypoteneuse.  A vector can be seen as the product of its unit vector and its magnitude; as such many people see a vector's magnitude as its scale.  The normal of the zero vector is undefined, in the way that dividing by zero is undefined, and will throw an arithmetic exception. ```1> scutil:normalize_vector([0,3,4]).
%% [0.0,0.6,0.8]'''<span style="color:red">TODO: When tuple comprehensions are introduced to the language, convert this to using them.</span>

%% @since Version 85

normalize_vector(VX) when is_list(VX) ->
    VM = vector_magnitude(VX),
    [ X / VM || X <- VX ];

normalize_vector(VX) when is_tuple(VX) -> list_to_tuple(normalize_vector(tuple_to_list(VX))).





%% @spec amean_vector_normal(VX::numeric_list()) -> number()

%% @doc {@section Statistics} Returns the arithmetic mean of the elements of the unit vector for the vector provided.

%% @since Version 85

amean_vector_normal(VX) -> arithmetic_mean(normalize_vector(VX)).





%% @spec gmean_vector_normal(VX::numeric_list()) -> number()

%% @doc {@section Statistics} Returns the geometric mean of the elements of the unit vector for the vector provided.

%% @since Version 85

gmean_vector_normal(VX) ->  geometric_mean(normalize_vector(VX)).





%% @spec hmean_vector_normal(VX::numeric_list()) -> number()

%% @doc {@section Statistics} Returns the harmonic mean of the elements of the unit vector for the vector provided.

%% @since Version 85

hmean_vector_normal(VX) ->   harmonic_mean(normalize_vector(VX)).





% Create sorted list X of 3-ary tuples {K,Ai,Bi} from sorted lists A, B of 2ary {K,Ai}/{K,Bi} tuples, where key K appears in both A and B

shared_keys(TupleList) when is_list(TupleList) ->
    {A,B} = lists:unzip(TupleList),
    shared_keys(lists:sort(A),lists:sort(B)).

%% @type keylist() = keylist().  All members of keylists are tuples of two-or-greater arity, and the first element is considered their key in the list.  List keys are unique; therefore `[{a,1},{b,1}]' is a keylist, but `[{a,1},{a,1}]' is not.
%% @type sorted_keylist() = keylist().  A sorted keylist is a keylist in the order provided by {@link lists:sort/1}.  Because of erlang tuple ordering rules and the fact that keylist keys are unique, this means the list will be ordered by key.

%% @equiv shared_keys(lists:zip(lists:sort(A), lists:sort(B)))
%% @spec shared_keys(TupleList::sorted_keylist(), Presorted::presorted) -> sorted_keylist()
%% @doc Equivalent to {@link shared_keys/1}, but skips sorting the lists (and thus requires pre-sorted lists), which may save significant work repetition.

shared_keys(TupleList, presorted) when is_list(TupleList) ->
    {A,B} = lists:unzip(TupleList),
    shared_keys(A,B);

%% @doc Create sorted list X of 3-ary tuples `{K,Ai,Bi}' from sorted lists A, B of 2ary `{K,Ai}'/`{K,Bi}' tuples, where key `K' appears in both `A' and `B'.

shared_keys(A,B) when is_list(A), is_list(B) ->
    both_lists_next_item(lists:sort(A),lists:sort(B),[]).

%% @equiv shared_keys(lists:sort(A),lists:sort(B))
%% @spec shared_keys(A::sorted_keylist(), B::sorted_keylist(), Presorted::presorted) -> sorted_keylist()
%% @doc Equivalent to {@link shared_keys/2}, but skips sorting the lists (and thus requires pre-sorted lists), which may save significant work repetition.

shared_keys(A,B,presorted) when is_list(A), is_list(B) ->
    both_lists_next_item(A,B,[]).

both_lists_next_item([],             _,              Work) -> lists:reverse(Work);
both_lists_next_item(_,              [],             Work) -> lists:reverse(Work);
both_lists_next_item([ {K,Ai} | Ar], [ {K,Bi} | Br], Work) -> both_lists_next_item(Ar, Br, [{K,Ai,Bi}]++Work);

both_lists_next_item(IA,             IB,             Work) ->
    [{Ka,_}|Ar] = IA,
    [{Kb,_}|Br] = IB,
    if
        Ka < Kb -> both_lists_next_item(Ar, IB, Work);
        true    -> both_lists_next_item(IA, Br, Work)
    end.





% collects results; do not use for huge lists

%% @deprecated Use {@link combinations/2} instead.

%% @spec all_unique_pairings(List::list()) -> tuple_list()

%% @doc {@section List} Generate every unique pair of elements from a list; deprecated in favor of {@link combinations/2}, which supports more than just two-element combinations and returns a list of lists instead of a list of tuples.  ```1> scutil:all_unique_pairings([a,b,c]).
%% [{b,c},{a,b},{a,c}]'''

%% @since Version 31

all_unique_pairings(A) when is_list(A) -> all_unique_pairings(A,[]).

all_unique_pairings([],      Work) -> Work;
all_unique_pairings([Ai|Ar], Work) -> all_unique_pairings(Ar, [{Ai,Ari}||Ari<-Ar] ++ Work).





% used for side effects, doesn't gather results; appropriate for enormous lists

% comeback

walk_unique_pairings([],    _) -> ok;
walk_unique_pairings([A|R], F) when is_function(F) ->
    walk_unique_pairings(A, R, F),
    walk_unique_pairings(R, F).

walk_unique_pairings(_A, [],     _F) -> ok;
walk_unique_pairings( A, [Rh|Rr], F) ->
    F(A,Rh),
    walk_unique_pairings(A, Rr, F).





%% @spec list_to_number(X::list()) -> number()

%% @doc {@section Conversion} Converts a list into a number; integers will be returned if there is no mantissa in the list representation. ```1> scutil:list_to_number("2").
%% 2
%%
%% 2> scutil:list_to_number("2.0").
%% 2.0
%%
%% 3> scutil:list_to_number("2.1").
%% 2.1'''

%% @since Version 8

list_to_number(X) ->
    case catch list_to_float(X) of
        {'EXIT',_} -> list_to_integer(X);
        Y -> Y
    end.





%% @equiv start_register_if_not_running(node(), Name, Module, Function, [])
start_register_if_not_running(Name, Module, Function) -> start_register_if_not_running(node(), Name, Module, Function, []).

%% @equiv start_register_if_not_running(node(), Name, Module, Function, Args)
start_register_if_not_running(Name, Module, Function, Args) -> start_register_if_not_running(node(), Name, Module, Function, Args).

%% @spec start_register_if_not_running(Node::atom(), Name::atom(), Module::atom(), Function::atom(), Args::list()) -> pid() | ok

%% @doc {@section Documentary} Check whether a process is registered locally, and if not, spawn it with a give function and arguments.  ```1> whereis(test).
%% undefined
%%
%% 2> scutil:start_register_if_not_running(node(), test, scutil, wait_until_terminate, []).
%% ok
%%
%% 3> whereis(test).
%% <0.726.0>
%%
%% 4> test ! terminate.
%% terminate
%%
%% 5> whereis(test).
%% undefined
%%
%% 6> scutil:start_register_if_not_running(node(), test, scutil, wait_until_terminate, []).
%% true
%%
%% 7> whereis(test).
%% <0.731.0>
%%
%% 8> scutil:start_register_if_not_running(node(), test, scutil, wait_until_terminate, []).
%% ok
%%
%% 9> whereis(test).
%% <0.731.0>'''

%% @since Version 8

start_register_if_not_running(Node, Name, Module, Function, Args) when is_atom(Name), is_atom(Module), is_atom(Function), is_list(Args) ->

    case whereis(Name) of
        undefined -> register(Name, spawn(Node, Module, Function, Args)), ok;
        _         -> ok
    end.





%% @spec counter(Name::any()) -> number()

%% @doc {@section Counters} Checks a counter's value; if the counter was not already defined, it will report zero. ```1> scutil:counter(hello).
%% 0
%%
%% 2> scutil:inc_counter(hello).
%% 1
%%
%% 3> scutil:inc_counter(hello).
%% 2
%%
%% 4> scutil:inc_counter(hello).
%% 3
%%
%% 5> scutil:counter(hello).
%% 3
%%
%% 6> scutil:reset_counter(hello).
%% 0
%%
%% 7> scutil:counter(hello).
%% 0'''

%% @since Version 54

counter(Name) ->

    start_register_if_not_running(scutil_counter_process, scutil, counter_process, []),
    scutil_counter_process ! {self(), get_counter, Name},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





%% @equiv adjust_counter(Name,1)
inc_counter(Name)    -> adjust_counter(Name, 1).

%% @equiv adjust_counter(Name,By)
inc_counter(Name,By) -> adjust_counter(Name, By).

%% @equiv adjust_counter(Name,-1)
dec_counter(Name)    -> adjust_counter(Name, -1).

%% @equiv adjust_counter(Name,-1*By)
dec_counter(Name,By) -> adjust_counter(Name, -1*By).





%% @spec adjust_counter(Name::any(), By::number()) -> number()

%% @doc {@section Counters} Adds to a counter's value; if the counter was not already defined, it will become the value in the `By' argument. ```1> scutil:counter(hello).
%% 0
%%
%% 2> scutil:inc_counter(hello).
%% 1
%%
%% 3> scutil:adjust_counter(hello, 3).
%% 4'''

%% @since Version 54

adjust_counter(Name, By) when is_number(By) ->

    start_register_if_not_running(scutil_counter_process, scutil, counter_process, []),
    scutil_counter_process ! {self(), adjust_counter, Name, By},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





%% @equiv set_counter(Name, 0)

%% @since Version 54

reset_counter(Name) -> set_counter(Name, 0).





%% @spec set_counter(Name::any(), To::number()) -> 0

%% @doc {@section Counters} Sets a counter's value to a specific value. ```1> scutil:counter(hello).
%% 0
%%
%% 2> scutil:set_counter(hello,4).
%% 4
%%
%% 3> scutil:counter(hello).
%% 4
%%
%% 4> scutil:reset_counter(hello).
%% 0
%%
%% 5> scutil:counter(hello).
%% 0'''

%% @since Version 54

set_counter(Name, To) when is_number(To) ->

    start_register_if_not_running(scutil_counter_process, scutil, counter_process, []),
    scutil_counter_process ! {self(), set_counter, Name, To},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





%% @private

counter_process() ->
    receive
        shutdown -> ok;
        {Caller, get_counter, Name} ->
            case get(Name) of
                undefined -> Caller ! {counter_at, Name, 0}, put(Name,0), counter_process();
                Defined   -> Caller ! {counter_at, Name, Defined},        counter_process()
            end;
        {Caller, adjust_counter, Name, By} ->
            case get(Name) of
                undefined ->                 Caller ! {counter_at, Name, By},  put(Name,By),  counter_process();
                Defined   -> New=Defined+By, Caller ! {counter_at, Name, New}, put(Name,New), counter_process()
            end;
        {Caller, set_counter, Name, To} ->
            Caller ! {counter_at, Name, To},
            case To of
                0 -> erase(Name);
                T -> put(Name,T)
            end,
            counter_process()
    end.





%% @equiv wait_until_terminate(quiet)
wait_until_terminate() -> wait_until_terminate(quiet).

%% @spec wait_until_terminate(IsQuiet::atom()) -> ok

wait_until_terminate(quiet) ->
    receive
        terminate -> ok;
        _         -> wait_until_terminate(quiet)
    end;

wait_until_terminate(loud) ->
    receive
        terminate -> ok;
        X         -> io:format("Received ~p~n", [X]), wait_until_terminate(loud)
    end.





module_has_function(Module, Function) ->

    scutil:deprecate("module_has_function() is deprecated in favor of erlang:function_exported/3"),
    lists:keymember(Function, 1, apply(Module, module_info, [exports])).





%% @spec shuffle(List::list()) -> list()

%% @doc {@section Random} Return a list with the original list's shallow members in a random order.  Deep lists are not shuffled; `[ [a,b,c], [d,e,f], [g,h,i] ]' will never produce sublist reorderings (`[b,c,a]') or list mixing (`[b,g,e]'), only reordering of the three top level lists.  The output list will always be the same length as the input list.  Repeated items and mixed types in input lists are safe. ```1> scutil:shuffle(lists:seq(1,9)).
%% [8,4,7,9,5,2,6,1,3]
%%
%% 2> {TheFaces, TheSuits} = {  [ace] ++ lists:seq(2,10) ++ [jack,queen,king],  [hearts,spades,clubs,diamonds]  }
%% {[ace,jack,queen,king,2,3,4,5,6,7,8,9,10],
%%  [hearts,spades,clubs,diamonds]}
%%
%% 3> Deck = scutil:shuffle([ {Face,Suit} || Face <- TheFaces, Suit <- TheSuits ]).
%% [ {6,spades}, {7,hearts}, {8,clubs}, {queen,spades}, {6,diamonds}, {ace,...}, {...} | ...]
%%
%% 4> scutil:shuffle([ duck,duck,duck,duck, goose ]).
%% [duck,goose,duck,duck,duck]'''
%%
%% <i>Originally found at <a href="http://wiki.trapexit.org/index.php/RandomShuffle">http://wiki.trapexit.org/index.php/RandomShuffle</a>; refactored for clarity, and unnecessary repeat nesting behavior removed.</i>

%% @since Version 8

shuffle(List) ->
   WeightedAndShuffled        = lists:map( fun(Item) -> { random:uniform(), Item } end, List ),
   { _, SortedAndDeweighted } = lists:unzip(lists:keysort(1, WeightedAndShuffled)),
   SortedAndDeweighted.





%% @private

% Handler must be no_handler_pid or { handler, PID [, idhandle] }

call_after_worker(MS, Func, Args, Handler) ->

    receive after MS ->

        case Func of
            { Module, FuncName } -> Result = apply(Module, FuncName, Args);
            FuncName             -> Result = apply(FuncName, Args)
        end,

        case Handler of
            { handler, PID, Handle } -> PID ! { call_after_result, Result, Handle };
            { handler, PID }         -> PID ! { call_after_result, Result };
            no_handler_pid           -> ok
        end

    end.





%% @equiv call_after(Length, Func, [],   {handler,self()})
call_after(Length, Func)                -> call_after(Length, Func, [],   {handler,self()}).

%% @equiv call_after(Length, Func, Args, {handler,self()})
call_after(Length, Func, Args)          -> call_after(Length, Func, Args, {handler,self()}).

%% @type handler() = { handler, PID } | { handler, PID, IdHandle } | no_handler_pid.  Use a handler() to provide handling behavior to functions like {@link call_after/1}.  The PID given is the pid to which result messages will be sent.  If an IdHandle is given, that IdHandle is passed back with the result, to pass extra information back to identify which result it is.  The atom no_handler_pid specifies that you would prefer the result to be discarded.

%% @spec call_after(Length::integer(), Func::function(), Args::atom(), Handler::handler()) -> { ok, spawned_worker, Worker::pid() }

%% @doc {@section Utility} Spawns a side process to non-blockingly make a call after a specified delay.  Will send the result as a message `{ call_after_result, Result }' to the handler process, which is the calling process unless otherwise specified (with {handler,OtherPid} or the atom no_handler_pid).  Delayed return value can include an ID, in the format `{ call_after_result, Result, IdHandle }', if the Handler PID is specified `{handler,PID,ID}', to help distinguish between returned calls if needed. ```1> Dbl = fun(X) -> X*2 end.
%% #Fun<erl_eval.6.13229925>
%%
%% 2> scutil:call_after(1000, Dbl, [3]).
%% {ok,spawned_worker,<0.3810.0>}
%%
%% 3> scutil:receive_one().
%% {item,{call_after_result,6}}
%%
%% 4> Receiver = fun() -> io:format("Listening.~n"), receive X -> io:format("Got ~p~n", [X]), ok end end.
%% #Fun<erl_eval.20.67289768>
%%
%% 5> R = spawn(Receiver).
%% Listening.
%% <0.3817.0>
%%
%% 6> scutil:call_after(1000, Dbl, [3], {handler, R}).
%% {ok,spawned_worker,<0.3829.0>}
%% Got {call_after_result,6}'''

%% @since Version 26

call_after(Length, Func, Args, Handler) ->

    Worker = spawn(?MODULE, call_after_worker, [Length, Func, Args, Handler]),
    { ok, spawned_worker, Worker }.





%% @equiv permute(List, length(List))

permute(List) -> permute(List, length(List)).

%% @type positive_integer() = integer().  Positive integer must be greater than zero.

%% @spec permute(List::list(), Depth::positive_integer()) -> list()

%% @doc {@section Utility} Calculate either the full or the depth-limited permutations of a list, order sensitive; contrast {@link combinations/2}.  Permutations are all valid orderings of a set of tokens; the permutations of `[a,b]' for example are `[a,b]' and `[b,a]'.  Depth limitation means the permutations of a smaller count of tokens from the main set; the 2-limited permutations of `[a,b,c]' for example are `[a,b]', `[a,c]', `[b,a]', `[b,c]', `[c,a]' and `[c,b]'.  Permutations are not ordered.  Mixed-type lists are safe; items are shallow evaluated, meaning that sublists within the list are treated as single elements, and will neither be rearranged nor will have elements selected from within them. ```1> scutil:permute(["dave","kate","pat"]).
%% [{"pat","kate","dave"}, {"kate","pat","dave"}, {"pat","dave","kate"}, {"dave","pat","kate"}, {"kate","dave","pat"}, {"dave","kate","pat"}]
%%
%% 2> scutil:permute([fast, strong, smart, lucky], 2).
%% [{strong,fast}, {smart,fast}, {lucky,fast}, {fast,strong}, {smart,strong}, {lucky,strong}, {fast,smart}, {strong,smart}, {lucky,smart}, {fast,lucky}, {strong,lucky}, {smart,lucky}]'''

%% @since Version 17

permute(List, 1)     when is_list(List)                    -> [ [T]    || T <- List ];
permute(List, Depth) when is_list(List), is_integer(Depth) -> [ [T]++R || T <- List, R <- permute(List--[T], Depth-1) ].





has_bit(Num, Bit) when is_integer(Num), is_integer(Bit), Num > 0, Bit >= 0, Bit < 64 -> (Num band (1 bsl Bit)) > 0.





count_bits(Num) when is_integer(Num), Num > 0 ->

    length([S || S <- lists:seq(0,63), has_bit(Num, S) == true]).





diff_timestamp({AM,AS,AU}, {BM, BS, BU}) ->

    ((BM-AM) * 1000000) + (BS-AS) + ((BU-AU)/1000000).





benchmark(Module, Func, Args) ->

    Start  = now(),
    Result = apply(Module, Func, Args),
    End    = now(),

    { diff_timestamp(Start,End), Result }.





expand_label({Label,List}) when is_list(List) -> [ {Label,L} || L<-List ];
expand_label({Label,Item})                    -> {Label, Item}.

expand_labels(List)        when is_list(List) -> lists:flatten([ expand_label(X) || X <- List ]).





% Thanks for some math help on erl-b, erl-c and engset, Vat and Wintermute

erlang_b_distribution(N,A) ->

   Num = math:pow(A,N) / scutil:factorial(N),
   Denom = lists:sum([ math:pow(A,I) / scutil:factorial(I) || I <- lists:seq(0,N) ]),

   Num / Denom.





erlang_c_distribution(N,A) ->

   Num = (math:pow(A,N) / scutil:factorial(N)) * (N/(N-A)),
   Denom = lists:sum([ math:pow(A,I) / scutil:factorial(I) || I <- lists:seq(0,N-1) ]) + ((math:pow(A,N)/scutil:factorial(N))*(N/(N-A))),

   {wait_probability, Num / Denom}.





% thanks for a much better implementation, etnt
implode(Separator, Data) when is_list(Data) andalso is_list(Separator) -> lists:foldr(fun(Item,[]) -> Item; (Item,Acc) -> Item ++ Separator ++ Acc end, "", Data).





% Thank god they called it rem.  -5 mod 2 is 1, not -1.  Most CPUs implement remainder and call it modulus.
% This is real modulus.
mod(Base, Range) when is_integer(Base), is_integer(Range) ->

    case Base rem Range of
        X when X < 0 -> X + Range;
        Z            -> Z
    end.





% Just:
%   1) put    -svn_revision("$+Revision$").    in your code after removing the plus (without the plus the example gets converted, sorry)
%   2) set the svn property svn:keywords to include at least Revision (case sensitive), and
%   3) check in the code.
% Magic follows.

scan_svn_revision(Module) ->

    "$Revision: " ++ X = get_module_attribute(Module, svn_revision),
    [ Head | _Rem ]    = string:tokens(X, " "),
    list_to_integer(Head).





%% @spec median_absolute_deviation(List::numericlist()) -> number()

%% @doc {@section Statistics} Calculate the median absolute deviation of a {@type numericlist()}. ```1> scutil:median_absolute_deviation([1,1,2,2,4,6,9]).
%% 1'''

%% @since Version 81

median_absolute_deviation(List) when is_list(List) ->

    ListMedian = scutil:median(List),
    scutil:median( [ abs(ListItem - ListMedian) || ListItem <- List ] ).





%% @spec make_notebook(Notebook::filename()) -> ok | { error, E }

%% @doc {@section Persistence} <span style="color:#888;font-style:italic">(Called automatically)</span> Creates a "notebook", which is just a convenience wrapped DETS set table.

%% @since Version 83

make_notebook(Notebook) -> ok.





%% @spec remove_notebook(Notebook::filename()) -> ok | { error, E }

%% @doc {@section Persistence} Attempts to remove a notebook; all data in the removed notebook is lost permanently.

%% @since Version 83

remove_notebook(Notebook) -> ok.





%% @spec has_notebook(Notebook::filename()) -> true | false

%% @doc {@section Persistence} Returns a boolean true or false whether a notebook under a given filename already exists.  Does not attempt to create the notebook under any circumstances.

%% @since Version 83

has_notebook(Notebook) -> ok.





%% @spec annote(Notebook::filename(), NoteName::any(), NewValue::any()) -> ok | { error, E }

%% @doc {@section Persistence} Stores a key/value pair to a notebook file, overwriting a match existing key if present; if the notebook does not exist, it is automatically created.    DETS opening and closing are automatically managed.

%% @since Version 83

annote(Notebook, NoteName, NewValue) -> annote(Notebook, [{NoteName, NewValue}]).

% @spec annote(Notebook::filename(), List::kv_list()) ->

annote(Notebook, NameValuePair)  when is_list(Notebook), is_tuple(NameValuePair) -> annote(Notebook, [NameValuePair]);
annote(Notebook, NameValuePairs) when is_list(Notebook), is_list(NameValuePairs) ->

    get_notebook_table(Notebook),
    [dets:insert(Notebook, {Term, Value}) || {Term, Value} <- NameValuePairs],
    close_notebook_table(Notebook).




%% @spec read_note(Notebook::filename(), NoteName::any()) -> { value, V } | { error, E }

%% @doc {@section Persistence} Read a key/value pair from a notebook file.  DETS opening and closing are automatically managed.

%% @since Version 83

read_note(Notebook, NoteName) when is_list(Notebook) ->

    get_notebook_table(Notebook),

    CurrentConfig = case dets:match(Notebook, {NoteName, '$1'}) of
        []    -> undefined;
        [[X]] -> { value, X }
    end,

    close_notebook_table(Notebook),
    CurrentConfig.






%% @spec has_note(Notebook::filename(), NoteName::any()) -> true | false | { error, E }

%% @doc {@section Persistence} Checks if a given note exists in a given notebook, returning boolean atoms as an answer.  Checking whether a note exists in a missing notebook is considered an error.  DETS opening and closing are automatically managed.

%% @since Version 83

has_note(Notebook, NoteName) -> ok.





%% @spec remove_note(Notebook::filename(), NoteName::any()) -> ok | { error, E }

%% @doc {@section Persistence} Removes a note by name from a given notebook.  Removing a nonexistant note is considered an error.

%% @since Version 83

remove_note(Notebook, NoteName)  when is_atom(Notebook), is_atom(NoteName)  -> remove_note(Notebook, [NoteName]);
remove_note(Notebook, NoteNames) when is_atom(Notebook), is_list(NoteNames) ->

    get_notebook_table(Notebook),
    [dets:delete(Notebook, NoteName) || NoteName <- NoteNames],
    close_notebook_table(Notebook).





%% @private
get_notebook_table(TableName) when is_list(TableName) -> dets:open_file(TableName, [{type, set}]).

%% @private
close_notebook_table(TableName) when is_list(TableName) -> dets:close(TableName).











%%%%%%%%%%%%%%%%%%%%
%%
%%   TODO CODE





% memory() ->
%
%    receive
%
%        terminate -> ok;
%
%        { Sender, store, Key, Val } ->
%            put(Key, Val),
%            Sender ! { memory_set_to, Key, Val },
%            memory();
%
%        { Sender, fetch, Key } ->
%            Sender ! { memory_found, get(Key) },
%            memory();
%
%    end.
%
%
%
%
% test() ->
%
%    register(mem, spawn(?MODULE, memory, [])),
%
%     mem ! { self(), store, foo, bar },
%     io:format("~p", [receive X -> X end]),
%
%     mem ! { self(), fetch, foo },
%     io:format("~p", [receive X -> X end]).





% Rename around the "annote" family

%install(ConfigName)                when is_atom(ConfigName)                                                                 -> install(ConfigName, []).
%install(ConfigName, DefaultConfig) when is_atom(ConfigName) andalso (is_list(DefaultConfig) orelse is_tuple(DefaultConfig)) ->
%
%    case is_installed(ConfigName) of
%
%        false -> configure(ConfigName, DefaultConfig), { ok, now_installed };
%        true  -> { error, already_installed, uninstall_first }
%
%    end.
%
%
%
%
%
%is_installed(ConfigName) when is_atom(ConfigName) ->
%
%    case dets:is_dets_file(sanitize_filename(ConfigName)) of
%
%        { error, _ } -> false;
%        _            -> true
%
%    end.
%
%
%
%
%
%uninstall(ConfigName) when is_atom(ConfigName) ->
%
%
%    case is_installed(ConfigName) of
%
%        false -> { error, not_installed };
%        true  ->
%
%            file:delete(sanitize_filename(ConfigName)),
%            { ok, uninstalled }
%
%    end.
%
%
%
%
%
%get_config(ConfigName) when is_atom(ConfigName) ->
%
%    get_table(ConfigName),
%    CurrentConfig = dets:match(sanitize_filename(ConfigName), '$1'),
%    close_table(ConfigName),
%    CurrentConfig.





%% @equiv map_reduce(Function, Workload, 1, nodes())
map_reduce(Function, Workload) -> map_reduce(Function, Workload, 1, nodes()).

%% @equiv map_reduce(Function, Workload, JobsPerNode, nodes())
map_reduce(Function, Workload, JobsPerNode) -> map_reduce(Function, Workload, JobsPerNode, nodes()).

%% @spec map_reduce(Function::function(), Workload::list(), JobsPerNode::positive_integer(), Nodes::list()) -> list()

%% @doc {@section Parallelism} Takes a workload, a function, a count of jobs per node and a node list, and distributes work on demand to available nodes according to completion until the master workload is fulfilled.  Results are provided in the order the workload was provided, regardless of in what order they are completed or received. ```
%% '''<span style="color:red">TODO: add crash handling behavior, progress querying behavior</span>
map_reduce(Function, Workload, JobsPerNode, Nodes) ->

    Computers      = lists:flatten(lists:duplicate(JobsPerNode, Nodes)),
    WorkOut        = [],
    TaggedWorkload = lists:zip(lists:seq(1,length(Workload)), Workload),
    WorkDone       = [],

    map_reduce_worker(Function, TaggedWorkload, Computers, WorkOut, WorkDone).





map_reduce_worker(_Function, [],             _Computers, [],      WorkDone) -> {_,Out} = lists:unzip(lists:keysort(1,WorkDone)), Out;                               % no work left, no work out?  done.
map_reduce_worker( Function, [],              Computers, WorkOut, WorkDone) -> map_reduce_wait_for_work(Function, [],             Computers, WorkOut, WorkDone);    % no work left, work out?  wait.
map_reduce_worker( Function, TaggedWorkload, [],         WorkOut, WorkDone) -> map_reduce_wait_for_work(Function, TaggedWorkload, [],        WorkOut, WorkDone);    % work left, no computers left?  wait.
map_reduce_worker( Function, TaggedWorkload,  Computers, WorkOut, WorkDone) -> map_reduce_do_work(      Function, TaggedWorkload, Computers, WorkOut, WorkDone).    % work left, computers left?  do work.





map_reduce_wait_for_work(Function, TaggedWorkload, Computers, WorkOut, WorkDone) ->

    receive
        { work_done, Computer, Tag, Result } -> map_reduce_worker(Function, TaggedWorkload, Computers++[Computer], WorkOut--[Tag], WorkDone++[{Tag,Result}])
    end.





map_reduce_do_work(Function, [{Tag,Workload}|RemWorkload], [Computer|RemComputers], WorkOut, WorkDone) ->

    spawn(Computer, fun(Who,What,Which,With) -> Who ! {work_done, node(), Which, apply(What,With) } end, [self(),Function,Tag,Workload]),
    map_reduce_worker(Function, RemWorkload, RemComputers, WorkOut++[Tag], WorkDone).






%receive_all_answers(MessageList, ProcessList) when length(MessageList) /= length(ProcessList) -> { error, message_list_and_process_list_must_have_same_length };
%receive_all_answers(MessageList, ProcessList) when is_list(MessageList), is_list(ProcessList) ->
%
%    IDs = lists:seq(1,length(MessageList)),
%    [ spawn(?MODULE, receive_all_answers_worker, [self(), ID, Message, Process]) || { ID, Message, Process } <- lists:zip3(IDs, MessageList, ProcessList) ],
%    wait_on_answers(IDs, []).
%
%
%
%
%
%receive_all_answers_worker(Collector, Id, Message, Process) ->
%
%    Process ! Message,
%    receive
%        X -> Collector ! { an_answer, Id, X }
%    end.
%
%
%
%
%
%wait_on_answers([],        Work) -> {_, Out} = lists:keysort(1,Work), Out;
%wait_on_answers(Remaining, Work) ->
%
%    receive
%        { an_answer, AnswerID, Answer } -> wait_on_answers(Remaining -- [AnswerID], Work++{AnswerID,Answer});
%        Other                           -> { error, { misunderstood_result, Other }, { work_completed, Work }, { work_remaining, Remaining } }
%    end.






%start() ->
%
%    case gen_tcp:listen(25,[]) of
%
%        { ok, ListeningSocket } -> { ok, listening_on_pid, spawn(?MODULE, accept_loop, [ListeningSocket]) };
%        { error, E }            -> { error, E }
%
%    end.
%
%
%
%
%
%accept_loop(ListeningSocket) ->
%
%    case gen_tcp:accept(ListeningSocket) of
%
%        { ok, ConnectedSocket } ->
%            spawn(?MODULE, handler_loop, [ConnectedSocket]),
%            accept_loop(ListeningSocket);
%
%        { error, E } ->
%            accept_loop(ListeningSocket)
%
%    end.
%
%
%
%
%
%handler_loop(ConnectedSocket) ->
%
%    receive
%
%        terminate              -> ok;
%        { tcp, Socket, Input } -> gen_tcp:send(Socket, "You said " ++ Input ++ "\r\n"), handler_loop(ConnectedSocket);
%        { error, E }           -> { error, E }
%
%    end.





%% @type list_of_lists() = list().  Every member of a {@type list_of_lists()} is a {@type list()}.

%% @spec combinations(Items::list(), OutputItemSize::positive_integer()) -> list_of_lists()

%% @doc {@section List} Provides a list of every unique combination of input terms, order-ignorant; contrast {@link permute/2}.  Permutations are all unique combinations of a set of tokens; the 2-permutations of `[a,b,c]' for example are `[a,b]', `[a,c]' and `[b,c]'.  Note the absence of other orderings, such as `[b,a]', which are provided by {@link permute/2}.  Combinations are taken of a smaller count of tokens than the main set.  Combinations are not ordered, but this implementation happens to provide answers in the same order as the input list.  Mixed-type lists are safe; items are shallow evaluated, meaning that sublists within the list are treated as single elements, and will neither be rearranged nor will have elements selected from within them. ```1> scutil:combinations([a,b,c,d],2).
%% [[a,b],[a,c],[a,d],[b,c],[b,d],[c,d]]
%%
%% 2> scutil:combinations(["dave","kate","pat"],2).
%% [["dave","kate"],["dave","pat"],["kate","pat"]]
%%
%% 3> scutil:combinations([fast, strong, smart, lucky], 2).
%% [[fast,strong], [fast,smart], [fast,lucky], [strong,smart], [strong,lucky], [smart,lucky]]''' {@section Thanks} to Alisdair Sullivan for this implementation, which has been slightly but not significantly modified since receipt.

%% @since Version 89

combinations(Items, 1) when is_list(Items) -> Items;
combinations([],   _N)                     -> [];
combinations(Items, N) when is_list(Items), is_integer(N), N > 0 -> [ lists:append( [lists:nth(I, Items)], [J] )          ||
                                                                      I <- lists:seq(1, length(Items)),
                                                                      J <- combinations( lists:nthtail(I, Items), (N-1) )  ].
