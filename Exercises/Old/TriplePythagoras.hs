triple x y = if x > y
  then print [((x^2)-(y^2)),( (2*x)*y), ((x^2)+(y^2))]
  else print "Unable to do triple"


main = do
  triple 3 1
