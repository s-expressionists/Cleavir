## Introduction

*This system currently only works in SBCL*

Usage:

```lisp
(asdf:load-system "cleavir-bir-visualizer")
(load ".../Cleavir/Environment/Examples/sbcl.lisp")
(cleavir.bir.visualizer:run)
```

[[file:documentation/screenshot.png]]

Then edit the form and/or change the optimization settings and watch the intermediate representations change.

Click objects to expand and collapse. BIR blocks have context menu (right pointer button click) entries "Organize slots by class" and "Flat list of slots" which display the instance slots of the respective object an addition to the specialized default presentation.
