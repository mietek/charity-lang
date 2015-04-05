Modal Mu
========

> _**NOTE:**  This is an archival version of a [page from the Charity website](http://pll.cpsc.ucalgary.ca/charity1/www/modelChecking.html)._
>
> _Unfortunately, the Modal Mu files have been lost ([#1](https://github.com/mietek/charity-language/issues/1))._


Model checking is likely the most widely used formal method. The essential idea is to model a piece of software or hardware as a finite labelled transition system which is then checked to see if the given property holds for all reachable states. Using this technique, it is possible to ensure, for example, that two or more components never simultaneously write to a system bus.

Model checking tells us whether a property holds of a given model. As models get more complicated, their size goes up. An alternative to model checking is proof; To appreciate the difference, consider how a model checker would handle, P AND Q IMPLIES P First, it would collect all states satisfying P, then all states satisfying Q, find their intersection, then check that these are a subset of the states satisfying P. This is surely an expensive endeavour if the number of states is large. Alternatively, the proof looks like:

```
   P |- P
 P,Q |- P
P/\Q |- P
```

Reading top to bottom:

* the top line says that P entails P,
* the second rule (called weakening) adds an extra assumption (Q)
* the third rule takes the conjunction of the two assumptions.

Hence, P AND Q entails P.

Proof can tell us whether a property holds of all models, without reference to the model. It can therefore even handle infinite models. Unfortunately, to describe a particular model, the number of assumptions needed would rival the size of the model.

I have built sequent checkers (aka a decision procedure); a HML checker, a Modal Mu checker, and a Circuit Logic Checker. To get the HML checker, with documentation and examples, click ~~here~~. To get the modal Mu checker, with documentation and examples, click ~~here~~. To get the Circuit checker, with some examples, click ~~here~~. The Circuit checker is a work in progress and includes a scanner, parser, pretty printer for terms, pretty printer for proofs, and counter examples. The documentation is from the Hml section. Cut elimination and documentation will be added later. Fixed points are not included.


---

Return to the [Charity](background.md) website.
