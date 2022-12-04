type shape = Circle of float
              | Rectangle of (float * float)
              | ComplexShape of shape list
;;

[Circle 2.;Circle 3.;Rectangle (3.,4.);ComplexShape[Circle 2.; Rectangle(1.,1.)]]
