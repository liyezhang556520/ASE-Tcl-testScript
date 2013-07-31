#!/usr/local/bin/tclsh8.6
set n 3

proc fun {depth} {
 global n
 set level [info level]
for {set i 1} {$i<$level} {incr i} {
  puts "Level $i:[info level $i]"
 }
 if {$depth==0} {
    return $n
 }
 #set depth [expr $depth-1]
 #puts $depth
 return [expr [fun [expr $depth-1]]+1]
}

proc addddd {} {
  return  [fun 4]
}

puts [addddd]
#puts [fun 4]
