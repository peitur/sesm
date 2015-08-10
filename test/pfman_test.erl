-module( pfman_test ).

-define( TEST1_SAMPLE,     [14,21,32,14,5,36,17,28,19,10,11,12,31,41,15,16,7,18,9]  ).
-define( TEST1_SAMPLE_OK1, [15,22,33,15,6,37,18,29,20,11,12,13,32,42,16,17,8,19,10] ).
-define( TEST1_FUN, fun(E) -> E + 1 end ).

-define( NUM_WORKERS, 3 ).

-compile( export_all ).


fun_test() ->
	case lists:map( ?TEST1_FUN, ?TEST1_SAMPLE ) of
		?TEST1_SAMPLE_OK1 -> ok;
		Else -> false
	end.



pfmap_test_ok( ) ->
	pfmap_test_ok( ?TEST1_FUN, ?TEST1_SAMPLE ).

pfmap_test_ok( Fun, List ) ->
	case sesm_util:pfmap( Fun, List, [{num_processes, ?NUM_WORKERS}] ) of

		Data -> Data

	end.


pfmap_insert_test( ) ->
	sesm_util:pfmap_insert( ?TEST1_SAMPLE, 100, 50 ).