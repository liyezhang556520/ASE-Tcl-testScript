#!/usr/local/bin/tclsh8.6
expr {srand(0)}
package require sqlite3
sqlite3 db :memory:
set rexpr {
  a b c d e
  a-b b-c c-d d-e
  a+b*2 a+b*2+c*3 a+b*2+c*3+d*4 a+b*2+c*3+d*4+e*5
  (a+b+c+d+e)/5
  abs(a) abs(b-c)
  {(SELECT count(*) FROM t1 AS x WHERE x.b<t1.b)}
  {(SELECT count(*) FROM t1 AS x WHERE x.c>t1.c AND x.d<t1.d)}
  {CASE WHEN c>(SELECT avg(c) FROM t1) THEN a*2 ELSE b*10 END}
  {CASE WHEN a<b-3 THEN 111 WHEN a<=b THEN 222
        WHEN a<b+3 THEN 333 ELSE 444 END}
  {CASE a+1 WHEN b THEN 111 WHEN c THEN 222
        WHEN d THEN 333  WHEN e THEN 444 ELSE 555 END}
}
puts [llength $rexpr]
puts [expr rand()]
puts [expr rand()]
puts [expr rand()]
puts [expr rand()]
puts [expr rand()]
puts [expr rand()]
puts [expr rand()]
puts [expr rand()]
puts [expr rand()]
puts [expr rand()]
puts [expr rand()]
puts {hash-threshold 8}


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

# Construct the schema.  9 tables, each with 5 integer columns and one
# text column.
#
for {set tn 1} {$tn<=9} {incr tn} {
#  puts {statement ok}
  set sql [subst {CREATE TABLE t${tn}(
  a$tn INTEGER,
  b$tn INTEGER,
  c$tn INTEGER,
  d$tn INTEGER,
  e$tn INTEGER,
  x$tn VARCHAR(30)
)}]
#  puts $sql
  db eval $sql
#  puts {}
}




for {set tn 1} {$tn<=9} {incr tn} {set nrow($tn) 0}
for {set i 0} {$i<1000} {incr i} {
  set tn [expr {int(rand()*9)+1}]
  set base 0 ;# [expr {$tn*1000}]
  incr nrow($tn)
  set x "table tn$tn row $nrow($tn)"
  foreach column {a b c d e} {
    set v [expr {$base+int(rand()*999)}]
    lappend cdata($column$tn) $v
    set $column $v
  }
  lappend tdata($tn) [list $a $b $c $d $e $x]
  set sql "INSERT INTO t$tn VALUES($a,$b,$c,$d,$e,'$x')"
  db eval $sql
#  puts "statement ok\n$sql\n"
}




# Create indices
#
# t1 gets prefix indices...
#
for {set i 0} {$i<5} {incr i} {
  puts "statement ok"
  set sql \
    "CREATE INDEX t1i$i ON t1([join [lrange {a1 b1 c1 d1 e1 x1} $i end] ,])"
  puts "$sql\n"
  db eval $sql
}




