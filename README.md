Themplate
=========

[![Build Status](https://travis-ci.org/bennofs/themplate.png?branch=master)](https://travis-ci.org/bennofs/themplate)

This is a very simple program for creating project templates. 

Installing Themplate
--------------------

### From hackage:

```
$ cabal install themplate
```

### From repository:

Checkout the source code:

```
$ git clone https://github.com/bennofs/themplate
$ cd themplate
```

Build in a cabal sandbox and install:

```
$ cabal sandbox init
$ cabal install --prefix=~/.cabal   # Install themplate executable to ~/.cabal/bin/themplate
```

Using Themplate
---------------

To use a exisiting template with themplate, just run:

```
$ themplate init <project name> <template name>
```

This will initialize the template with the given name in a new directory with the name of the project.

To list all available templates, run:

```
$ themplate list
```

Configuration file
------------------

The configuration file is read using configurator, the syntax is described [here](http://hackage.haskell.org/package/configurator-0.2.0.2/docs/Data-Configurator.html).
The options are passed to the template, so they can use patterns to include user-specific information. An example configuration file could look like this:

```
user { 
  name = "Benno Fünfstück"
  email = "benno.fuenfstueck@gmail.com"
}

github {
  user = "bennofs"
}
```

This will make the options `user.name`, `user.email` and `github.user` available to templates.

Creating templates
------------------

A template is just a subdirectory in `~/.themplate`. When a new project is created with the template, the files in that directory are
copied to the project directory. The files contents and the file names in the template can contain patterns. Patterns are enclosed
in `{{` and `}}`. In a pattern, the following special forms are substituted:

- `$$name$$` will be substituted for the value of the configuration option `name`. An error is thrown if the configuration option doesn't exist.
- `??name??` will be substituted for the value of the configuration option `name`. If the configuration option doesn't exist, the pattern will
   evaluate to the empty string.

There is one special configuration option, `project.name`. This will always be set to the current project's name. `??name??` patterns are checked prior to ``$$name$$`` patterns, so the following:

```
{{homepage:      http://github.com/??github.user??/$$project.name$$/}}
```

won't throw an error if `github.user` is unset, even if `project.name` was unset.

You can find an example template at https://github.com/bennofs/dotfiles/tree/master/.themplate/.
