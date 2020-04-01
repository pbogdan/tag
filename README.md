# Tag

A simple utility to deal with media files tags. Born out of personal need `tag` can manipulate (read
& write) media file tags as well as help renaming files based on the tags. Currently only tested
with my personal music collection.

`tag` uses the [htaglib](https://github.com/mrkkrp/htaglib) Haskell library by Mark Karpov to access media file tag information.

## Installation

At the moment using Nix package manager is the only supported method of building and installing
`tag`. `nix-build` ran from the top level directory will build the project which can then, if
desired, can be installed into a user profile using `nix-env`.


## Usage

`tag` supports three modes of operation:

- `read` prints out tags from the specified media files
- `write` is capable of modifying media tags
- `rename` renames files based on media tags

### `read`

```
$ tag read [-f|--format format] file
```

Print media tag information of the specified file(s), optionally based on a custom
[format](#format). If the format is not specified prints out all of the details. Currently this is
just a handful of entries the author finds most useful.

### `write`

```
tag write ((-a|--artist artist) | (-t|--title title) |
           (-l|--album album) | (-c|--comment comment) |
           (-g|--genre genre) | (-y|--year year) | (-n|--track track))
           file
```

Alter media tag information for the specified file(s). At least one of the options needs to be
specified. Hopefully the option name should be self-explanatory.

### `rename`

```
$ tag rename (-f|--format format) file
```

Rename the file(s) based on the [format](#format) specifier.


### <a name="format"></a>`format` option

Format is a user specified template format to be used with the `read` and `rename` commands. It
accepts literal strings as well as variables based on the current file information. It also accepts
a number of filters.

Full (low-level) details of the templating engine are available through
[EDE](https://hackage.haskell.org/package/ede) Haskell library documentation.


Literal strings can be embedded as is. For example

```
$ tag read --format "some text" file.mp3
```

prints out literal `some text`.

#### Variables

Variables can be embedded using `{{ variable }}` syntax. For example

```
$ tag read --format "{{ artist }}" file.mp3
```

prints out the artist of the file.

Currently supported variables based on the tags of the current file are:

- `title`
- `artist`
- `album`
- `comment`
- `genre`
- `year`
- `track`

There are also a few "special" variables:

- `filename` - file name of the current file
- `filepath` - the full path of the current file
- `all` - a special variable that evaluates to all of the known tag information

#### Filters

Filters use the `|` syntax and can be applied to both literals as well as variables.

For example:

```
tag read --format "{{ artist | toLower }}" file.mp3
```

prints out the artist in all lower case.

Currently a full list of filters is not documented.
