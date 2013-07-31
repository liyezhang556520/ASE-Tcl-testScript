#!/usr/local/bin/tclsh8.6
#
# Run this script to generate a prototype test script for
# Single Table ASE query.
#
# This is the original test script generator.  It generates a single
# table T1(a,b,c,d,e,x) with 5 INTEGER columns 1 VARCHAR column and 30 rows of
# data, then does various queries against that table.
#
expr {srand(0)}

# Scramble the $inlist into a random order.
#
proc scramble {inlist} {
  set y {}
  foreach x $inlist {
    lappend y [list [expr {rand()}] $x]
  }
  set y [lsort $y]
  set outlist {}
  foreach x $y {
    lappend outlist [lindex $x 1]
  }
  return $outlist
}

puts {CREATE TABLE t1(a INTEGER, b INTEGER, c INTEGER, d INTEGER, e INTEGER, x VARCHAR(30));}
puts {}

set nrow 0
for {set i 0} {$i<30} {incr i} {
  set base [expr {$i/3+int(rand()*15)}]
  set values {}
  for {set j 0} {$j<5} {incr j} {
    if {rand()<0.05} {
      lappend values NULL
    } else {
      lappend values [expr {int($j*rand()*10)+$base}]
    }
  }
  incr nrow
  set x "row $nrow"
  set values [scramble $values]
  set cols [scramble {a b c d e}]
  set sql "INSERT INTO t1([join $cols ,],x) VALUES([join $values ,],'$x');"
#  puts "statement ok"
  puts $sql
  puts ""
}

set rexpr {
  a b c d e
  a-b b-c c-d d-e
  a+b*2 a+b*2+c*3 a+b*2+c*3+d*4 a+b*2+c*3+d*4+e*5
  (a+b+c+d+e)/5
  abs(a) abs(b-c)
  {(SELECT count(*) FROM t1  x WHERE x.b<t1.b)}
  {(SELECT count(*) FROM t1  x WHERE x.c>t1.c AND x.d<t1.d)}
  {CASE WHEN c>(SELECT avg(c) FROM t1) THEN a*2 ELSE b*10 END}
  {CASE WHEN a<b-3 THEN 111 WHEN a<=b THEN 222
        WHEN a<b+3 THEN 333 ELSE 444 END}
  {CASE a+1 WHEN b THEN 111 WHEN c THEN 222
        WHEN d THEN 333  WHEN e THEN 444 ELSE 555 END}
}
set nrexpr [llength $rexpr]
set sequence {}
set type {}
for {set i 1} {$i<=$nrexpr} {incr i} {
  lappend sequence $i
  append type I
}
set wexpr {
  a>b
  b>c
  c>d
  d>e
  {a IS NULL}
  {b IS NOT NULL}
  coalesce(a,b,c,d,e)<>0
  {c BETWEEN b-2 AND d+2}
  {d NOT BETWEEN 110 AND 150}
  {e+d BETWEEN a+b-10 AND c+130}
  {(a>b-2 AND a<b+2)}
  {(e>a AND e<b)}
  {(c<=d-2 OR c>=d+2)}
  {(e>c OR e<d)}
  {EXISTS(SELECT 1 FROM t1 AS x WHERE x.b<t1.b)}
}
set nwexpr [llength $wexpr]

proc select_clause {depth UpSelColList indent} {
  global rexpr
  set selectSql {}
  set boolNeed(groupby) 0
  if {$depth == 0} { return "" }
  set currSelColList {}
  if {[llength $UpSelColList] == 0} {
    set selColNums [expr {int(rand()*5)+1}]
  } else {
    set selColNums [expr {int(rand()*3)}]
    foreach x $UpSelColList {
      lappend currSelColList $x
    }
  }
  foreach x [lrange [scramble $rexpr] 1 $selColNums] {
    lappend currSelColList $x
  }
  set p [expr rand()]
  set selClause "SELECT "
  if {$p<0.3333} {
    append selClause "ALL "
  } elseif {$p<0.6666} {
    append selClause "DISTINCT "
  }
  append selClause " [join $currSelColList ",\n       $indent"]"

# add two parentheses for subquery
  if {[info level] != 1} {
    append selectSql "($selClause"
  } else {
    append selectSql $selClause
  }

# From clause  
  set fromExpr {}
  set retFromExprGen [from_expr_gen $depth $currSelColList $indent]
  append fromExpr [lindex $retFromExprGen 0]
  set availbleCols [lindex $retFromExprGen 1]
  append selectSql "\n  $indent FROM " $fromExpr
  
# WHERE clause
  set whereClause {}
  if {rand()<0.5} {
    set whereClause "\n  $indent WHERE "
    append whereClause [where_expr_gen ]
  }
  append selectSql $whereClause

# GROUP BY clause
  set groupByClause {}
  if {$boolNeed(groupby) == 1} {
    set groupByClause "\n  $indent GROUP BY "
    append groupByClause [groupby_expr_gen ]
  } else {
    if {rand()<0.5} {
      set groupByClause "\n  $indent GROUP BY "
      append groupByClause [groupby_expr_gen ]
    } else {
      # nothing to do
    }
  }
  append selectSql $groupByClause

# HAVING clause
  set havingClause {}
  if {rand()<0.5} {
    set havingClause "\n  $indent HAVING "
    append havingClause [having_expr_gen ]
  }
  append selectSql $havingClause
    
# ORDER BY clause
  set orderbyClause {}
  if {rand()<0.5} {
    set orderbyClause "\n  $indent ORDER BY "
  }
  append selectSql $orderbyClause


# corresponding prior adding two parentheses for subquery
  if {[info level] != 1} {
    append selectSql ")"
  } else {
    append selectSql ""
  }
  
  set retSelStruc {}
  lappend retSelStruc $selectSql
  lappend retSelStruc $currSelColList
  return $retSelStruc

};# end of select_clause func


# auto generate FROM expression
proc from_expr_gen {depth selColList indent} {
  set fromExpr {}
  set colFromTbls {}
  while {1} {
    if {rand()<0.5} {
      append fromExpr "t1"
      foreach x [list a b c d e] {
        lappend colFromTbls $x
      }
      break
    } else {
      if {$depth <= 1} { continue }
      append indent "       " 
      set retSelectClause [select_clause [expr $depth-1] $selColList $indent]
      append fromExpr [lindex $retSelectClause 0]
      foreach x [lindex $retSelectClause 1] {
        lappend colFromTbls $x
      }
      break
    }
  }
  set retFromExprStruc {}
  lappend retFromExprStruc $fromExpr
  lappend retFromExprStruc $colFromTbls
  return $retFromExprStruc
}


# auto generate WHERE expression
proc where_expr_gen {} {
  return a>b
}

# auto generate GROUP BY expression
proc groupby_expr_gen {} {
  return a,b,c,d,e
}

# auto generate HAVING expression
proc having_expr_gen {} {
  return a>b
}

# auto generate ORDER BY expression
proc orderby_expr_gen {} {
  return a,b
}





for {set i 0} {$i<10} {incr i} {
puts [lindex [select_clause 4 {} {}] 0]
puts {}
puts "========================================================="
puts {}
}
   




