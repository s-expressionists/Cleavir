This is a sham system that facilitates generating Cleavir's documentation. It is an extension to [Staple](https://github.com/Shinmera/staple); if you are looking for a general documentation generation system for use with your own code, Staple is what Cleavir is using.

# Generating the documentation

Ensure the `staple-markdown` system is loaded. Then simply

```lisp
(staple:generate :cleavir-documentation-generation :if-exists :supersede)
```

## Developer details

This system is empty - as you can see from the ASDF system definition, it includes no files and has no dependencies. Loading it won't do you any good. The documentation generation is set up in `staple.ext.lisp`, which Staple will automatically load in the course of the above `generate` call.

`staple.ext.lisp` defines a moderately extensive extension to Staple in order to improve its handling of Cleavir, which is a very complex set of systems. Unlike usual Staple, this documentation generator produces a hierarchy of `staple:project` instances rather than just one.

Cleavir systems may put in further extensions particular to those systems with their own `staple.ext.lisp` files. It is recommended that these be written in the `cleavir-documentation-generation` package defined here. The extension has been written such that the usual Staple generic functions are still used, so you can define methods as needed, although you probably shouldn't further customize `staple:find-project`. Systems should make sure their pages are instances of `cleavir-page` in order for source code links to work correctly.

# Generating the CSS

The CSS file used for the documentation is generated from the [LASS](https://github.com/Shinmera/LASS) file. To recompile,

```lisp
(lass:generate (asdf:system-relative-pathname :cleavir-documentation-generation "top.lass"))
```

# Copyright

`top.ctml` and `top.lass` are modifications of Staple's `default/default.ctml` and `default/default.lass` respectively. The original files were authored by Nicolas Hafner (a.k.a. [Shinmera](https://github.com/Shinmera)), and the modified files are used here under the zlib license. Here is the original notice:

```
Copyright (c) 2014 Nicolas Hafner

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
   claim that you wrote the original software. If you use this software
   in a product, an acknowledgment in the product documentation would be
   appreciated but is not required.
2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.
3. This notice may not be removed or altered from any source distribution.
```
