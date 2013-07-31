#!/usr/local/bin/tclsh8.6
# Run this script to generate a large prototype test script for
# Panthera ASE Project.
#
# This is the original test script generator.  It generates a single
# table T1(a,b,c,d,e) with all INTEGER columns and 30 rows of non-NULL
# data, then does various queries against that table.
#
expr {srand(0)}

# Scramble the $inlist list into a random order.
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

# Construct the schema.  9 tables, each with 5 integer columns and one
# text column.
#
for {set tn 1} {$tn<=9} {incr tn} {
  set sql [subst {CREATE TABLE t${tn}(
  a$tn INTEGER,
  b$tn INTEGER,
  c$tn INTEGER,
  d$tn INTEGER,
  e$tn INTEGER,
  x$tn VARCHAR(30)
)}]
  puts $sql
  puts {}
}

# Populate the tables with data
#
for {set tn 1} {$tn<=9} {incr tn} {set nrow($tn) 0}

for {set tn 1} {$tn<=9} {incr tn} {set nrow($tn) 0}
for {set i 0} {$i<300} {incr i} {
  set tn [expr {int(rand()*9)+1}]
  set base 0 ;# [expr {$tn*1000}]
  incr nrow($tn)
  set x "table tn$tn row $nrow($tn)"
  foreach column {a b c d e} {
    if {rand()<0.05} {
      set v NULL
    } else {
      set v [expr {$base+int(rand()*50)}]
    }
    lappend cdata($column$tn) $v
    set $column $v
  }
  lappend tdata($tn) [list $a $b $c $d $e $x]
  set sql "INSERT INTO t$tn VALUES($a,$b,$c,$d,$e,'$x')"
  puts "$sql\n"
}








