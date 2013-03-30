# nedfryst

*En av de mest revolutionerande upptäckterna före den stora pesten var stasisfältet.* -- Mutant 2, Världsboken

**An attempt to put Clojure namespaces, including anonymous functions, under stasis.**

Highly non-functional and untested. It's primary a spike to see if I can reduce the startup time of [Deuce](https://github.com/hraberg/deuce) by dumping its state after loadup, similar in spirit to how [Emacs duz it](http://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Emacs.html).

Attaches a Java agent so it can track various classes Clojure creates and store away the bytes.
Most sane approaches would lead elsewhere than here. But it's fun.

To run it, you first need to build the jar so the agent can attach:

    lein jar

Then:

```clj
(in-ns 'nedfryst.core)
(enable-freezing) ;; only needed if you need to freeze non-compiled fns.

(in-ns 'user)
(def x 6)
(defn y [] x) ;; ... which we do.

(in-ns 'nedfryst.core)
(freeze (the-ns 'user) "target/test.ser")
(remove-ns 'user) ;; or bounce the REPL to ensure the class of y is really gone.
(thaw "target/test.ser")

(in-ns 'user)
(y)
;=> 6
```

## References

[Kryo](https://code.google.com/p/kryo/) Nathan Sweet, 2008-13 "Fast, efficient Java serialization and cloning"

[Creation, dynamic loading and instrumentation with javaagents](http://dhruba.name/2010/02/07/creation-dynamic-loading-and-instrumentation-with-javaagents/) Dhruba Bandopadhyay, 2010

[DMTCP: Distributed MultiThreaded CheckPointing](http://dmtcp.sourceforge.net/) Jason Ansel et al. 2008-13 - ".. is a tool to transparently checkpoint the state of multiple simultaneous applications"

[Carbonite](https://github.com/revelytix/carbonite) Revelytix, 2011 - Clojure serialization library which uses Kryo 1.


## License

Copyright © 2013 Håkan Råberg

Distributed under the Eclipse Public License, the same as Clojure.
