-module( sesm_actions ).

-export([
		action/5
	]).



action( Expected, Currnt, New, apache, Options ) -> ok;


action( Expected, Currnt, New, Job, Options ) -> ok.
