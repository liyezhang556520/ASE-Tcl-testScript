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


set selColsCTypeExpr {
  EMPNUM EMPNAME CITY PNUM
}

set selColsDTypeExpr {
  HOURS GRADE
} 


set singleValueSelExpr {
  {SELECT COUNT(*) FROM STAFF}
  {SELECT COUNT(*) FROM STAFF WHERE ABS(GRADE) = 12}
  {SELECT COUNT(*) FROM STAFF WHERE GRADE > 10}
  {SELECT COUNT(*) FROM STAFF WHERE GRADE < 10}
  {SELECT COUNT(*) FROM STAFF WHERE CITY = 'Greenmount' OR GRADE = 15}
  {SELECT COUNT(*) FROM STAFF WHERE GRADE = 26}
  {SELECT COUNT(*) FROM   STAFF WHERE  GRADE=130}
  {SELECT COUNT(*) FROM STAFF WHERE GRADE = 11}
  {SELECT AVG(GRADE) FROM STAFF}
  {SELECT COUNT(*) FROM STAFF WHERE EMPNUM  NOT LIKE '_36'}
  {SELECT COUNT(*) FROM STAFF WHERE NOT(EMPNUM  LIKE '_36')}
  {SELECT COUNT(*) FROM STAFF WHERE CITY IS NOT NULL}
  {SELECT COUNT(*) FROM STAFF WHERE NOT (CITY IS NULL)}
  {SELECT COUNT(*) FROM STAFF WHERE STAFF.CITY = (SELECT PROJ.CITY FROM PROJ WHERE PNUM > 'P7')}
  {SELECT COUNT(*) FROM STAFF WHERE NOT (STAFF.CITY = (SELECT PROJ.CITY FROM PROJ WHERE PNUM > 'P7' ))}
  {SELECT COUNT(*) FROM STAFF WHERE 40 IN (SELECT HOURS FROM WORKS WHERE STAFF.EMPNUM = WORKS.EMPNUM)}
  {SELECT COUNT(*) FROM STAFF WHERE 40 NOT IN (SELECT HOURS FROM WORKS WHERE STAFF.EMPNUM = WORKS.EMPNUM)}
  {SELECT COUNT(*) FROM STAFF WHERE EXISTS (SELECT * FROM WORKS WHERE HOURS = 40 AND STAFF.EMPNUM = WORKS.EMPNUM)}
  {SELECT COUNT(*) FROM STAFF WHERE NOT EXISTS (SELECT * FROM WORKS WHERE HOURS = 40 AND STAFF.EMPNUM = WORKS.EMPNUM)}
  {SELECT COUNT(*) FROM STAFF WHERE EMPNAME = 'Ed '}
  {SELECT COUNT (*) FROM STAFF WHERE EMPNUM IN (SELECT EMPNUM FROM WORKS)}
  {SELECT COUNT(*) FROM STAFF WHERE CITY = ALL (SELECT CITY FROM PROJ WHERE PNAME = 'SDP')}
  {SELECT COUNT(*) FROM STAFF WHERE CITY <> ALL (SELECT CITY FROM PROJ WHERE PNAME = 'SDP')}
  {SELECT MIN(PNAME) FROM PROJ, WORKS, STAFF WHERE PROJ.PNUM = WORKS.PNUM AND WORKS.EMPNUM = STAFF.EMPNUM AND BUDGET - GRADE * HOURS * 100 IN (-4400, -1000, 4000)}
  {SELECT COUNT(*) FROM WORKS WHERE HOURS BETWEEN -80 AND -40}
  {SELECT COUNT(*) FROM WORKS WHERE HOURS BETWEEN 11.999 AND 12 OR HOURS BETWEEN 19.999 AND 2.001E1}
  {SELECT COUNT(*) FROM PROJ WHERE PNUM <> ALL (SELECT PNUM FROM WORKS WHERE EMPNUM = 'E8')}
}

set singleColCTypeSelExpr {
  {SELECT EMPNUM FROM STAFF WHERE GRADE = (SELECT MAX(GRADE) FROM STAFF)}
  {SELECT EMPNUM FROM STAFF WHERE GRADE = (SELECT MIN(GRADE) FROM STAFF)}
  {SELECT CITY FROM STAFF WHERE GRADE NOT BETWEEN 12 AND 13}
  {SELECT CITY FROM STAFF WHERE NOT(GRADE BETWEEN 12 AND 13)}
  {SELECT STAFF.EMPNAME FROM STAFF WHERE STAFF.EMPNUM IN (SELECT WORKS.EMPNUM FROM WORKS WHERE WORKS.PNUM IN (SELECT PROJ.PNUM FROM PROJ WHERE PROJ.CITY='Tampa'))}
  {SELECT STAFF.EMPNAME FROM STAFF WHERE STAFF.EMPNUM = ANY (SELECT WORKS.EMPNUM FROM WORKS WHERE WORKS.PNUM IN (SELECT PROJ.PNUM FROM PROJ WHERE PROJ.CITY='Tampa'))}
  {SELECT EMPNAME FROM STAFF WHERE EMPNAME LIKE 'Al%'}
  {SELECT CITY FROM STAFF WHERE EMPNAME LIKE 'B__t%'}
  {SELECT CITY FROM STAFF WHERE CITY LIKE 'XiS___S%%' ESCAPE 'S'}
  {SELECT EMPNAME FROM STAFF WHERE CITY IS NULL}
  {SELECT STAFF.EMPNAME FROM STAFF WHERE NOT EXISTS (SELECT * FROM PROJ WHERE NOT EXISTS (SELECT * FROM WORKS WHERE STAFF.EMPNUM = WORKS.EMPNUM AND WORKS.PNUM=PROJ.PNUM))}
  {SELECT EMPNAME FROM STAFF WHERE GRADE < SOME (SELECT BUDGET/1000 - 39 FROM PROJ WHERE CITY='Deale')}
  {SELECT EMPNUM FROM STAFF WHERE GRADE < (SELECT MAX(GRADE) FROM STAFF)}
  {SELECT EMPNAME FROM STAFF WHERE EMPNUM IN (SELECT EMPNUM FROM WORKS WHERE PNUM = 'P2')}
  {SELECT EMPNAME FROM STAFF WHERE EMPNUM IN (SELECT EMPNUM FROM WORKS WHERE PNUM IN (SELECT PNUM FROM PROJ WHERE PTYPE = 'Design'))}
  {SELECT PNUM FROM PROJ WHERE PROJ.CITY = (SELECT STAFF.CITY FROM STAFF WHERE EMPNUM = 'E1')}
  {SELECT PNUM FROM PROJ WHERE PROJ.CITY = (SELECT STAFF.CITY FROM STAFF WHERE EMPNUM > 'E1' )}
  {SELECT CITY FROM   STAFF WHERE  EMPNAME NOT BETWEEN 'A' AND 'E'}
  {SELECT CITY FROM   STAFF WHERE  NOT( EMPNAME BETWEEN 'A' AND 'E' )}
  {SELECT EMPNAME FROM STAFF UNION SELECT EMPNAME FROM STAFF UNION ALL SELECT EMPNAME FROM STAFF}
  {SELECT EMPNAME FROM STAFF WHERE EMPNUM = (SELECT EMPNUM FROM WORKS GROUP BY EMPNUM, HOURS HAVING HOURS = 12)}
  {SELECT EMPNAME FROM STAFF WHERE (SELECT EMPNUM FROM WORKS WHERE PNUM = 'P3') = EMPNUM}
  {SELECT COALESCE (CITY, EMPNUM) FROM STAFF}
  {SELECT ALL EMPNUM FROM WORKS WHERE HOURS = 12}
  {SELECT PNUM FROM WORKS WHERE PNUM > 'P1' GROUP BY PNUM HAVING COUNT(*) > 1}
  {SELECT WORKS.PNUM FROM WORKS GROUP BY WORKS.PNUM HAVING WORKS.PNUM IN (SELECT PROJ.PNUM FROM PROJ GROUP BY PROJ.PNUM HAVING SUM(PROJ.BUDGET) > 25000)}
  {SELECT W1.EMPNUM FROM WORKS W1 WHERE W1.PNUM = 'P2' AND NOT EXISTS (SELECT * FROM WORKS W2 WHERE W2.EMPNUM = W1.EMPNUM AND W2.PNUM = 'P1')}
}

set singleColDTypeSelExpr {
  {SELECT HOURS FROM WORKS WHERE PNUM NOT IN (SELECT PNUM FROM WORKS WHERE PNUM IN ('P1','P2','P4','P5','P6'))}
}

set singleRowSelExpr {
  {SELECT SUM(HOURS), MAX(HOURS) FROM  STAFF, WORKS}
  {SELECT AVG(HOURS), MIN(HOURS) FROM  STAFF, WORKS WHERE STAFF.EMPNUM = 'E2' AND STAFF.EMPNUM = WORKS.EMPNUM}
  {SELECT SUM(HOURS),AVG(HOURS),MIN(HOURS),MAX(HOURS) FROM    WORKS WHERE   EMPNUM='E8' GROUP BY PNUM}
}

set multiColSelExpr {
  {SELECT EMPNUM, EMPNAME, GRADE, CITY FROM STAFF WHERE EMPNUM = 'E1'}
  {SELECT WORKS.EMPNUM FROM WORKS WHERE WORKS.PNUM = 'P2' UNION SELECT STAFF.EMPNUM FROM STAFF WHERE STAFF.GRADE=13}
  {SELECT WORKS.EMPNUM FROM WORKS WHERE WORKS.PNUM = 'P2' UNION ALL SELECT STAFF.EMPNUM FROM STAFF WHERE STAFF.GRADE = 13}
  {SELECT EMPNAME,PNUM,HOURS FROM STAFF,WORKS WHERE STAFF.EMPNUM = WORKS.EMPNUM UNION SELECT EMPNAME,PNUM,HOURS FROM STAFF,WORKS WHERE NOT EXISTS (SELECT HOURS FROM WORKS WHERE STAFF.EMPNUM = WORKS.EMPNUM)}
  {SELECT EMPNUM,EMPNAME,GRADE,STAFF.CITY, PNAME, PROJ.CITY FROM STAFF, PROJ WHERE STAFF.CITY = PROJ.CITY}
  {SELECT EMPNUM,EMPNAME,GRADE,STAFF.CITY,PNUM,PNAME, PTYPE,BUDGET,PROJ.CITY FROM STAFF, PROJ WHERE STAFF.CITY = PROJ.CITY AND GRADE <> 12}
  {SELECT DISTINCT STAFF.CITY, PROJ.CITY FROM STAFF, WORKS, PROJ WHERE STAFF.EMPNUM = WORKS.EMPNUM AND WORKS.PNUM = PROJ.PNUM}
  {SELECT FIRST1.EMPNUM, SECOND2.EMPNUM FROM STAFF FIRST1, STAFF SECOND2 WHERE FIRST1.CITY = SECOND2.CITY AND FIRST1.EMPNUM < SECOND2.EMPNUM}
  {SELECT * FROM STAFF WHERE GRADE <= (SELECT AVG(GRADE)-1 FROM STAFF)}
  {SELECT EMPNUM, EMPNAME FROM STAFF WHERE EMPNUM IN (SELECT EMPNUM FROM WORKS WHERE PNUM IN (SELECT PNUM FROM PROJ WHERE PTYPE IN (SELECT PTYPE FROM PROJ WHERE PNUM IN (SELECT PNUM FROM WORKS WHERE EMPNUM IN (SELECT EMPNUM FROM WORKS WHERE PNUM IN (SELECT PNUM FROM PROJ WHERE PTYPE = 'Design'))))))}
  {SELECT EMPNUM,CITY FROM   STAFF WHERE  EMPNUM='E1' OR NOT(EMPNUM='E1')}
  {SELECT EMPNUM,CITY FROM   STAFF WHERE  EMPNUM='E1' AND NOT(EMPNUM='E1')}
  {SELECT GRADE, HOURS, BUDGET FROM STAFF, WORKS, PROJ}
  {SELECT COL1, MAX(COL2) FROM VTABLE GROUP BY COL1 HAVING MAX(COL2) > ANY (SELECT GRADE FROM STAFF) AND MAX(COL2) < SOME (SELECT HOURS FROM WORKS)}
  {SELECT COL1, MAX(COL2) FROM VTABLE GROUP BY COL1 HAVING EXISTS (SELECT * FROM STAFF WHERE EMPNUM = 'E1') AND MAX(COL2) BETWEEN 10 AND 90}
  {SELECT PNUM, WORKS.EMPNUM, EMPNAME, HOURS FROM WORKS, STAFF WHERE STAFF.EMPNUM = WORKS.EMPNUM}
  {SELECT 'ZZ', EMPNUM, EMPNAME, -99 FROM STAFF WHERE NOT EXISTS (SELECT * FROM WORKS WHERE WORKS.EMPNUM = STAFF.EMPNUM)}
  {SELECT STAFF.EMPNUM, SUM(HOURS), MIN(HOURS) FROM  STAFF, WORKS GROUP BY STAFF.EMPNUM}
  {SELECT STAFF.EMPNUM, AVG(HOURS), MIN(HOURS) FROM  STAFF, WORKS WHERE STAFF.EMPNUM IN ('E1','E4','E3') AND STAFF.EMPNUM = WORKS.EMPNUM GROUP BY STAFF.EMPNUM HAVING COUNT(*) > 1}
  {SELECT * FROM STAFF}
  {SELECT EMPNUM, GRADE*1000 FROM STAFF WHERE GRADE * 1000 > ANY (SELECT SUM(BUDGET) FROM PROJ GROUP BY CITY, PTYPE HAVING PROJ.CITY = STAFF.CITY)}
  {SELECT CITY, COUNT(DISTINCT GRADE) FROM STAFF GROUP BY CITY}
  {SELECT STAFF.*, HOURS FROM STAFF, WORKS WHERE STAFF.EMPNUM = WORKS.EMPNUM OR EMPNAME = 'Carmen'}
  {SELECT STAFF.CITY,EMPNAME,PNAME,BUDGET FROM STAFF LEFT JOIN PROJ ON STAFF.CITY = PROJ.CITY AND STAFF.CITY <> 'Vienna' AND EMPNAME <> 'Don' WHERE BUDGET > 15000 OR BUDGET IS NULL}
  {SELECT STAFF.CITY,EMPNAME,PNAME,BUDGET FROM STAFF LEFT JOIN PROJ ON STAFF.CITY = PROJ.CITY AND STAFF.CITY <> 'Vienna' WHERE (BUDGET > 15000 OR BUDGET IS NULL) AND EMPNAME <> 'Don'}
  {SELECT * FROM STAFF LEFT OUTER JOIN WORKS USING (EMPNUM)}
  {SELECT EMPNUM, EMPNAME FROM STAFF WHERE EMPNUM IN (SELECT EMPNUM  FROM WORKS WHERE PNUM IN (SELECT PNUM  FROM PROJ WHERE PTYPE IN (SELECT PTYPE  FROM PROJ WHERE PNUM IN (SELECT PNUM  FROM WORKS WHERE EMPNUM IN (SELECT EMPNUM  FROM WORKS WHERE PNUM IN (SELECT PNUM   FROM PROJ WHERE PTYPE IN (SELECT PTYPE  FROM PROJ WHERE CITY IN (SELECT CITY  FROM STAFF WHERE EMPNUM IN (SELECT EMPNUM  FROM WORKS WHERE HOURS = 20 AND PNUM = 'P2' )))))))))}
  {SELECT EMPNUM, CITY FROM STAFF UNION SELECT PTYPE, CITY FROM PROJ}
  {SELECT EMPNUM,HOURS FROM WORKS WHERE PNUM='P2'}
  {SELECT EMPNUM,HOURS FROM WORKS WHERE PNUM = 'P2'}
  {SELECT PNUM,EMPNUM,HOURS FROM WORKS WHERE HOURS=80 UNION SELECT PNUM,EMPNUM,HOURS FROM WORKS WHERE HOURS=40 UNION SELECT PNUM,EMPNUM,HOURS FROM WORKS WHERE HOURS=20}
  {SELECT PNUM,EMPNUM,HOURS FROM WORKS WHERE HOURS=12 UNION ALL (SELECT PNUM,EMPNUM,HOURS FROM WORKS UNION SELECT PNUM,EMPNUM,HOURS FROM WORKS WHERE HOURS=80)}
  {SELECT EMPNUM,HOURS FROM WORKS WHERE EMPNUM = 'E1' AND PNUM = 'P4'}
  {SELECT EMPNUM,PNUM FROM   WORKS WHERE  HOURS IS NULL}
  {SELECT WORKS.HOURS FROM WORKS WHERE WORKS.PNUM NOT IN (SELECT PROJ.PNUM FROM PROJ WHERE PROJ.BUDGET BETWEEN 5000 AND 40000)}
  {SELECT EMPNUM, PNUM, HOURS FROM WORKS GROUP BY PNUM, EMPNUM, HOURS HAVING MIN(HOURS) > 12 AND MAX(HOURS) < 80}
  {SELECT PNUM, SUM(HOURS) FROM WORKS GROUP BY PNUM}
  {SELECT EMPNUM,HOURS FROM WORKS GROUP BY EMPNUM,HOURS}
  {SELECT EMPNUM,PNUM FROM   WORKS WHERE  HOURS <= ALL (SELECT AVG(HOURS) FROM   WORKS GROUP BY PNUM)}
  {SELECT EMPNUM,PNUM FROM   WORKS WHERE HOURS < (SELECT HOURS FROM WORKS WHERE EMPNUM = 'E8') OR NOT(HOURS < (SELECT HOURS FROM WORKS WHERE EMPNUM = 'E8'))}
  {SELECT EMPNUM,PNUM FROM   WORKS WHERE HOURS < (SELECT HOURS FROM WORKS WHERE EMPNUM = 'E8') AND NOT(HOURS< (SELECT HOURS FROM WORKS WHERE EMPNUM = 'E8'))}
  {SELECT PNUM, SUM(HOURS) FROM WORKS GROUP BY PNUM HAVING EXISTS (SELECT PNAME FROM PROJ WHERE PROJ.PNUM = WORKS.PNUM AND SUM(WORKS.HOURS) > PROJ.BUDGET / 200)}
  {SELECT PROJ.CITY AS PCITY, STAFF.CITY SCITY, BUDGET + GRADE * HOURS * 100  REAL_BUDGET FROM STAFF, PROJ, WORKS WHERE WORKS.EMPNUM = STAFF.EMPNUM AND WORKS.PNUM = PROJ.PNUM AND EMPNAME = 'Alice' AND PROJ.PNUM = 'P3'}
  {SELECT * FROM WORKS RIGHT JOIN PROJ ON WORKS.PNUM = PROJ.PNUM}
}

set selWithOrderbyExpr {
  {SELECT EMPNUM FROM STAFF WHERE GRADE = (SELECT MAX(GRADE) FROM STAFF) ORDER BY EMPNUM}
  {SELECT EMPNAME FROM STAFF WHERE EMPNUM IN (SELECT EMPNUM FROM WORKS WHERE PNUM = 'P2') ORDER BY EMPNAME}
  {SELECT COALESCE (CITY, EMPNUM) FROM STAFF ORDER BY 1}
  {SELECT W1.EMPNUM FROM WORKS W1 WHERE W1.PNUM = 'P2' AND NOT EXISTS (SELECT * FROM WORKS W2 WHERE W2.EMPNUM = W1.EMPNUM AND W2.PNUM = 'P1') ORDER BY 1 ASC}
  {SELECT WORKS.EMPNUM FROM WORKS WHERE WORKS.PNUM = 'P2' UNION SELECT STAFF.EMPNUM FROM STAFF WHERE STAFF.GRADE=13 ORDER BY 1 DESC}
  {SELECT EMPNUM, EMPNAME FROM STAFF WHERE EMPNUM IN (SELECT EMPNUM FROM WORKS WHERE PNUM IN (SELECT PNUM FROM PROJ WHERE PTYPE IN (SELECT PTYPE FROM PROJ WHERE PNUM IN (SELECT PNUM FROM WORKS WHERE EMPNUM IN (SELECT EMPNUM FROM WORKS WHERE PNUM IN (SELECT PNUM FROM PROJ WHERE PTYPE = 'Design')))))) ORDER BY EMPNUM}
  {SELECT COL1, MAX(COL2) FROM VTABLE GROUP BY COL1 HAVING MAX(COL2) > ANY (SELECT GRADE FROM STAFF) AND MAX(COL2) < SOME (SELECT HOURS FROM WORKS) ORDER BY COL1}
  {SELECT COL1, MAX(COL2) FROM VTABLE GROUP BY COL1 HAVING EXISTS (SELECT * FROM STAFF WHERE EMPNUM = 'E1') AND MAX(COL2) BETWEEN 10 AND 90 ORDER BY COL1}
  {SELECT PNUM, WORKS.EMPNUM, EMPNAME, HOURS FROM WORKS, STAFF WHERE STAFF.EMPNUM = WORKS.EMPNUM ORDER BY 2}
  {SELECT 'ZZ', EMPNUM, EMPNAME, -99 FROM STAFF WHERE NOT EXISTS (SELECT * FROM WORKS WHERE WORKS.EMPNUM = STAFF.EMPNUM) ORDER BY EMPNUM}
  {SELECT STAFF.EMPNUM, AVG(HOURS), MIN(HOURS) FROM  STAFF, WORKS WHERE STAFF.EMPNUM IN ('E1','E4','E3') AND STAFF.EMPNUM = WORKS.EMPNUM GROUP BY STAFF.EMPNUM HAVING COUNT(*) > 1 ORDER BY STAFF.EMPNUM}
  {SELECT STAFF.EMPNUM, AVG(HOURS), MIN(HOURS) FROM  STAFF, WORKS WHERE STAFF.EMPNUM IN ('E1','E4','E3') AND STAFF.EMPNUM = WORKS.EMPNUM GROUP BY STAFF.EMPNUM HAVING COUNT(*) > 1 ORDER BY STAFF.EMPNUM}
  {SELECT STAFF.CITY,EMPNAME,PNAME,BUDGET FROM STAFF LEFT JOIN PROJ ON STAFF.CITY = PROJ.CITY AND STAFF.CITY <> 'Vienna' AND EMPNAME <> 'Don' WHERE BUDGET > 15000 OR BUDGET IS NULL ORDER BY STAFF.CITY, EMPNAME, BUDGET}
  {SELECT STAFF.CITY,EMPNAME,PNAME,BUDGET FROM STAFF LEFT JOIN PROJ ON STAFF.CITY = PROJ.CITY AND STAFF.CITY <> 'Vienna' AND EMPNAME <> 'Don' WHERE BUDGET > 15000 OR BUDGET IS NULL ORDER BY STAFF.CITY, EMPNAME, BUDGET}
  {SELECT STAFF.CITY,EMPNAME,PNAME,BUDGET FROM STAFF LEFT JOIN PROJ ON STAFF.CITY = PROJ.CITY AND STAFF.CITY <> 'Vienna' WHERE (BUDGET > 15000 OR BUDGET IS NULL) AND EMPNAME <> 'Don' ORDER BY STAFF.CITY, EMPNAME, BUDGET}
  {SELECT EMPNUM,HOURS FROM WORKS WHERE PNUM='P2' ORDER BY EMPNUM DESC}
  {SELECT EMPNUM,HOURS FROM WORKS WHERE PNUM = 'P2' ORDER BY 2 DESC,EMPNUM DESC}
  {SELECT PNUM,EMPNUM,HOURS FROM WORKS WHERE HOURS=80 UNION SELECT PNUM,EMPNUM,HOURS FROM WORKS WHERE HOURS=40 UNION SELECT PNUM,EMPNUM,HOURS FROM WORKS WHERE HOURS=20 ORDER BY 3,1}
  {SELECT PNUM,EMPNUM,HOURS FROM WORKS WHERE HOURS=12 UNION ALL (SELECT PNUM,EMPNUM,HOURS FROM WORKS UNION SELECT PNUM,EMPNUM,HOURS FROM WORKS WHERE HOURS=80) ORDER BY 2,1}
  {SELECT * FROM WORKS RIGHT JOIN PROJ ON WORKS.PNUM = PROJ.PNUM ORDER BY 1 DESC, 2}
}

# the SELECT clause generate func
proc select_clause {depth UpSelColList indent} {
  global selColsCTypeExpr selColsDTypeExpr
  set selectSql {}
  set boolNeed(groupby) 0
  if {$depth == 0} { return "" }
  set currSelColList {}
  if {[llength $UpSelColList] == 0} {
    set selColNums [expr {int(rand()*3)+1}]
  } else {
    set selColNums [expr {int(rand()*3)}]
    foreach x $UpSelColList {
      lappend currSelColList $x
    }
  }
  foreach x [lrange [scramble [concat $selColsCTypeExpr $selColsDTypeExpr]] 1 $selColNums] {
    lappend currSelColList $x
  }

# From clause  
  set fromExpr {}
  set retFromExprGen [from_expr_gen $depth $currSelColList $indent]
  append fromExpr [lindex $retFromExprGen 0]
  set availbleCols [lindex $retFromExprGen 1]
  set availbleColsLen [llength $availbleCols]
  set selColListLen [expr {int(rand()*$availbleColsLen)}]
  set temp [scramble $availbleCols]
  set currSelColList [lrange $temp 0 $selColListLen]
  set otherAvailbleCols [lrange $temp [expr $selColListLen+1] [expr $availbleColsLen-1]]
  
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

  append selectSql "\n  $indent FROM " $fromExpr
  
# WHERE clause
  set whereClause {}
  if {rand()<0.5} {
    set whereClause "\n  $indent WHERE "
    append whereClause [where_expr_gen $availbleCols $indent]
  }
  append selectSql $whereClause

# GROUP BY clause
  set groupByClause {}
  set groupByExpr {}
  if {$boolNeed(groupby) == 1} {
    set groupByClause "\n  $indent GROUP BY "
    set groupByExpr [groupby_expr_gen $currSelColList $otherAvailbleCols]
    append groupByClause $groupByExpr
    set boolNeed(groupby) 1
  } else {
    if {rand()<0.5} {
      set groupByClause "\n  $indent GROUP BY "
      set groupByExpr [groupby_expr_gen $currSelColList $otherAvailbleCols]
      append groupByClause $groupByExpr
      set boolNeed(groupby) 1
    } else {
      # nothing to do
    }
  }
  append selectSql $groupByClause
  set groupbyCols [split $groupByExpr ,]

# HAVING clause
  set havingClause {}
  if {$boolNeed(groupby) == 1} {
    if {rand()<0.8} {
      set havingClause "\n  $indent HAVING "
      append havingClause [having_expr_gen $groupbyCols $indent]
   }
  }
  append selectSql $havingClause
    
# ORDER BY clause
  set orderbyClause {}
  if {rand()<0.5} {
    set orderbyClause "\n  $indent ORDER BY "
    append orderbyClause [orderby_expr_gen $availbleCols]
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
  global multiColSelExpr selWithOrderbyExpr
  set fromExpr {}
  set colFromTbls {}
  while {1} {
    set p [expr rand()]
    if {$p<0.2} {
      append fromExpr "staff "
      foreach x [list empnum empname city grade] {
        lappend colFromTbls $x
      }
      break
    } elseif {$p<0.4} {
      append fromExpr "staff, works "
      foreach x [list empname city grade pnum hours empnum] {
        lappend colFromTbls $x
      }
      break
    } elseif {$p<0.6} {
      append fromExpr "works "
      foreach x [list empnum pnum hours] {
        lappend colFromTbls $x
      }
      break
    } elseif {$p<0.8} {
      set fromSelClause [concat $multiColSelExpr $selWithOrderbyExpr]
      set nfromSelClause [llength $fromSelClause]
      append fromExpr "([lindex $fromSelClause [expr int(rand()*$nfromSelClause)]])"
      foreach x [list empname pnum city] {
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
proc where_expr_gen {whereAvailCols indent} {
  return [where-having_expr_gen $whereAvailCols $indent]
}

# auto generate GROUP BY expression
proc groupby_expr_gen {selListCols otherCols} {
  set retGroupbyExprGen {}
  set ncols [llength $otherCols]
  set n [expr int(rand()*$ncols)]
  set tmp [concat $selListCols [lrange [scramble $otherCols] 1 [expr int($n/[expr int(rand()*5+1)])]]]
  set retGroupbyExprGen [join [scramble $tmp] ,]
#puts "tmp length [llength $tmp], allcols [llength $allCols], selLicols [llength $selListCols] n is $n retlength is [llength regGroupbyExprGen]"  
  return $retGroupbyExprGen
}

# auto generate HAVING expression
proc having_expr_gen {havingAvailCols indent} {
  return [where-having_expr_gen $havingAvailCols $indent]
}

# auto generate ORDER BY expression
proc orderby_expr_gen {allCols} {
  set ncols [llength $allCols]
  set n [expr int(rand()*$ncols)]
  return [join [lrange [scramble $allCols] 0 $n] ,]
}

set valCTypeExpr {
  'E1' 'E2' 'E3' 'E4' 'E5' 'Alice' 'Betty' 'Carmen' 'Don' 'Ed' 'Alpha'
  'Deale' 'Vienna' Akron' 'P1' 'P2' 'P3' 'P4' 'P5' 'P6' 'P7'
}

set valDTypeExpr {
  1 4 0 3 100 13 12 16 10 20 40 80 60 10000 20000 30000 50000
}

set existsSelClauseList [concat $singleValueSelExpr [concat $singleColCTypeSelExpr [concat $singleColDTypeSelExpr [concat $singleRowSelExpr [concat $multiColSelExpr]]]]]

# auto gengerate expressions in WHERE and HAVING clause
proc where-having_expr_gen {availCols indent} {
  global valCTypeExpr valDTypeExpr singleValueSelExpr singleColCTypeSelExpr singleColDTypeSelExpr singleRowSelExpr multiColSelExpr existsSelClauseList
  set nvalCTypeExpr [llength $valCTypeExpr]
  set nvalDTypeExpr [llength $valDTypeExpr]
  set nsingleValueSelExpr [llength $singleValueSelExpr]
  set nsingleColCTypeSelExpr [llength $singleColCTypeSelExpr]
  set nsingleColDTypeSelExpr [llength $singleColDTypeSelExpr]
  set nsingleRowSelExpr [llength $singleRowSelExpr]
  set nmultiColSelExpr [llength $multiColSelExpr]
  set wherehavingExpr {}
  set nAvaliCols [llength $availCols]
  set loopCounts [expr int(rand()*4)]
  append indent "       "
  for {set i 0} {$i<$loopCounts} {incr i} {
    if {$i!=0} {
      if {rand()<0.3} {
        append wherehavingExpr "\n   $indent  AND  "
      } else {
        append wherehavingExpr "\n   $indent  OR  "
      }
    }
    set leftOperand {}
    set rightHandCond {}
    set leftOpIsDType 0
    set p [expr rand()]
    if {$p<0.1} {
      if {rand()<0.3} {
        set leftOperand [lindex [scramble $valDTypeExpr] 3]
        set leftOpIsDType 1
      } else {
        set leftOperand [lindex [scramble $valCTypeExpr] 3]
      }
    } elseif {$p<0.8} {
      set leftOperand [lindex [scramble $availCols] 0]
      set searchRet [lsearch [list grade hours] $leftOperand]
      if {$searchRet==1} {
        set leftOpIsDType 1
      }
    } else {
       set nexistsSelClauseList [llength $existsSelClauseList]
       set existsp [expr int(rand()*$nexistsSelClauseList)]
       append wherehavingExpr "exists ([lindex [scramble $existsSelClauseList] $existsp])"
       continue
    }
    
    if {$leftOpIsDType} {
      set rightHandCond [cond_right_side_gen 1]
    } else {
      set rightHandCond [cond_right_side_gen 0]
    }

    append wherehavingExpr "$leftOperand $rightHandCond "
  }
  
  return $wherehavingExpr
}

# a collection of compare operators
set compareOp {
  > < = <> != >= <=
}

# auto generate right hand condition expression in WHERE and HAVING clause
proc cond_right_side_gen {lOpIsDigitType} {
  set p [expr rand()]
#  set rightSideExpr {}
  if {$p<0.35} {
    return [compare_expr_gen $lOpIsDigitType]
  } else {
    return [logic_expr_gen $lOpIsDigitType]
  }
}

# auto generate compare expression
proc compare_expr_gen {leftOpIsDType} {
  global compareOp
  set compareExpr {}
  append compareExpr [lindex [scramble $compareOp] 3]
  
}

# auto generate logic expression
proc logic_expr_gen {leftOpIsDType} {
  set logicExpr {}
  set p [expr int(rand()*8)]
  if {$p<1} {
    append logicExpr "is NULL "
  } elseif {$p<2} {
    append logicExpr "is not NULL "
  } elseif {$p<3} {
    append logicExpr "in "
  } elseif {$p<4} {
    append logicExpr "not in "
  } elseif {$p<5} {
    append logicExpr "between "
  } elseif {$p<6} {
    append logicExpr "not between "
  } elseif {$p<7} {
    append logicExpr "like "
  } else {
    append logicExpr "not lik "
  }
}



# main loop
for {set i 0} {$i<100} {incr i} {
puts [lindex [select_clause 2 {} {}] 0]
puts {}
puts "========================================================="
puts {}
}
   


