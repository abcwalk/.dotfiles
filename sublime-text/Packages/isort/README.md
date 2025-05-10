Sublime text isort plugin
=========================

Small plugin that adds a command to sublime which replaces the contents with the output of the isort library.
See below for more information about the library

## Install

### Package Control

Available in [@wbond's package control][package-control].  Just bring up the package control menu in sublime (default `ctrl-shift-p`), and enter `Package Control: Install Package`, search for `isort`.

### Manual

Clone this repository from your Sublime packages directory:

#### Linux (untested)

```
$ cd ~/.config/sublime-text-2/Packages
$ git clone https://github.com/thijsdezoete/sublime-text-isort-plugin
```

#### OSX

```
$ cd "~/Library/Application Support/Sublime Text 2/Packages"
$ git clone https://github.com/thijsdezoete/sublime-text-isort-plugin
```

#### Windows

```
$ cd "%APPDATA%\Sublime Text 2"
$ git clone https://github.com/thijsdezoete/sublime-text-isort-plugin
```

isort your python imports for you so you don't have to.

isort is a Python utility / library to sort imports alphabetically, and automatically separated into sections.
It provides a command line utility, Python library, Vim plugin, Sublime plugin, and Kate plugin to quickly sort all your imports.

for more details on isort see [the isort repo][isort].

[isort]: https://github.com/timothycrosley/isort
[package-control]: https://packagecontrol.io/


Using isort
===========

By default isort command is run on save. To change this behaviour set your user/project settings to include:

    {
        "isort.sort_on_save": false,
    }

You can also run the command manually by bringing up the command menu, and select "isort: sort imports" command.

Note that the command is only visible, and runs on autosave, when used on "python files".
