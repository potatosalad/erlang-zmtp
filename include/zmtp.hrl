%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2014, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  15 Sep 2014 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------

-ifndef(ZMTP_HRL).

-define(INLINE_LOWERCASE_BC(Bin),
	<< << case C of
		$A -> $a;
		$B -> $b;
		$C -> $c;
		$D -> $d;
		$E -> $e;
		$F -> $f;
		$G -> $g;
		$H -> $h;
		$I -> $i;
		$J -> $j;
		$K -> $k;
		$L -> $l;
		$M -> $m;
		$N -> $n;
		$O -> $o;
		$P -> $p;
		$Q -> $q;
		$R -> $r;
		$S -> $s;
		$T -> $t;
		$U -> $u;
		$V -> $v;
		$W -> $w;
		$X -> $x;
		$Y -> $y;
		$Z -> $z;
		C -> C
	end >> || << C >> <= Bin >>).

-define(ZMTP_HRL, 1).

-endif.
