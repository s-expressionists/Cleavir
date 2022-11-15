It is sometimes useful to add mixins to a class without altering its definition. For example, this can be used to add information to [BIR](https://s-expressionists.github.io/Cleavir/cleavir-bir) objects particular to a client or extension, so that the general BIR definitions do not need to be aware of the client/extension code. The `cleavir-stealth-mixins` facility provides a small interface to do so reliably.

`define-stealth-mixin` defines a new class and makes it a superclass of an existing "victim" class. This new class is set up so that it will remain a superclass even if the definition of the victim class is reevaluated.

Original hack by Gilbert Baumann.
