
data test -> T = T0 | T1 | T2 | T3 | T4 | T5: 1 -> T.


def eq_test: test * test -> bool

           = (T0,   T0)  => true
           | (T1,   T1)  => true
           | (T2,   T2)  => true
           | (T3,   T3)  => true
           | (T4,   T4)  => true
           | (T5,   T5)  => true
           | _           => false.


def lt_test: test * test -> bool

           = (T0,   T0)  => false
           | (T0,   _ )  => true
           | (T1,   T0)  => false
           | (T1,   T1)  => false
           | (T1,   _ )  => true
           | (T2,   T0)  => false
           | (T2,   T1)  => false
           | (T2,   T2)  => false
           | (T2,   _ )  => true
           | (T3,   T0)  => false
           | (T3,   T1)  => false
           | (T3,   T2)  => false
           | (T3,   T3)  => false
           | (T3,   _ )  => true
           | (T4,   T0)  => false
           | (T4,   T1)  => false
           | (T4,   T2)  => false
           | (T4,   T3)  => false
           | (T4,   T4)  => false
           | (T4,   _ )  => true
           | _           => false.


def le_test = pt => or (lt_test pt, eq_test pt).
def gt_test = pt => not le_test pt.
def ge_test = pt => not lt_test pt.

