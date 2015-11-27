<script type="text/javascript">
document.getElementById("develop").className += " selected";
document.getElementById("develop_framework").className += " selected";
</script>


<!---
pandoc -c style.css -f markdown+header_attributes+fenced_code_blocks+line_blocks+pipe_tables -s -B md/html/title.html --toc --toc-depth=5 -B md/html/develop.html -o html/develop_framework.html md/develop_framework.md
-->

### Introduction

We provide some ... that can be used to quickly create a new widget or
object that integrates well into `loon`.

We use this method for code-reuse rather than any other object
oriented approach because `Tcl` version `8.5` does not come with any
object orient system (namely `oo`) as part of its core. And, we do not
want to create any dependencies in order to `loon` to run with the
`Tcl` distribution that comes bundled with some `R` versions.

### Example: Star Glyphs

We now show step by step how the `stars` object and its `view` widget
are created.

~~~
set d [dict create\
	  Age [list 20 25 16 35 42]\
	  Weight  [list 80 75 76 65.7 46]\
	  Sex [list m f f m m]\
	  Income [list 1000 1500 800 760 1100]]
~~~

This for the stars object every variable must be of the same length

~~~
dict for {name variable} $d {
	puts "$name: [llength $variable]"
} 
~~~

### Create Objects

### Create Widgets

#### TEMP create optimal

~~~
::loon::object stars namespace {data sequence axisDirections}
~~~

Now overwrite `Init`, `isvaild_option`, `set_option`

~~~
set t [::loon::type name]

~~~




creates everything (bindings, configure, isvalid, set, & namespace)


add binding






#### 


#### Stars Objects

The stars object then stores the data, sequence, and axis directions
for each variable in the sequence.

~~~
set s [::loon::stars]
$s configure -data $d -sequence [list Age Weight Sex Income Weight]
~~~

`::loon::stars` creates a procedure in the namespace that calls
`::loon::stars`. The procedure name is saved in `s` and is `::stars0`,
`::stars1`, ... . 

The stars objects support binding for when any of its data changes.

~~~
$s bind "data" {puts "Data in %O has changed"}
~~~

And similar as with the `::loon::plot` widget we want bindings for
some event pattern with

~~~
$s bind "data"
~~~

or add more code to be evaluated by a `data` event

~~~
$s bind "data" {+puts "More things to do."}
~~~

The `loon` help strings also support event logic involving `!,||,&&`

~~~
$s bind "(data||sequence)&&!axisDirections" {puts "Something to evaluate"}
~~~

#### Stars View Widgets

To look at the star glyphs defined in the stars object we provide a
stars view widget

~~~
set sv [::loon::stars::view -stars $s]
~~~

Which, like other `Tk` widgets has a widget path name here stored in
`sv` that is a procedure that provides the `configure` and `cget`
subcommands:

~~~
$sv configure -selected {0 0 0 1 0} -color {red red green green blue}
$sv cget -stars
~~~

`loon` widgets also provide bindings if their state changes

~~~
$sv bind "(selected||color)" {puts "Color or Selected State has changed"}
~~~

Note that this binding model is close to the `Tk` standard bind system
put fairly different than the one used with `loon` scatterplots.


We now show the details of implementing the functionality described
above using the `loon` helper strings.

#### Objects

To create a stars object we define a creator function `::loon::stars`
with

~~~
namespace eval loon {
  proc stars {{name ""}} \
       [string map [list %type stars %namespace ::loon::stars]\
           ${::loon::helper::object}]
}
~~~

The `object` string requires the existence of `%namespace::Init`
procedure, a `%namespace::State` variable, a `%namespace::commands`
and a defined `%type`. For the stars this means we must provide
`::loon::stars::Init`

~~~
namespace eval ::loon::stars {
	variable State

	proc Init {widget} {
		State(${widget}.data) ""
		State(${widget}.sequence) ""
		State(${widget}.axisDirections) ""
	}
}
~~~

The procedure call that `::loon::stars` returns calls
`::loon::stars::commands $widget $args`, so we must provide this
procedure

~~~
namespace eval ::loon::stars {
	proc commands {widget command args} {
		variable State
		switch -- $command {
			bind {bind $widget $args}
			configure {configure $widgets $args}
			cget {
				switch -- [lindex $args 0] {
					-stars {return $State(${widget}.stars)}
					-sequence {return $State(${widget}.sequence)}
					-axisDirections {return $State(${widget}.axisDirections)}
				}
            }
	        default {error "command $command does not exist.\
				Use configure or cget."}
		}
	}
}
~~~

The `commands` procedure can be used to completely define the API to
the object. The first subcommand we provided is `bind`. We can create
a binding behavior using the `bind` helper string

~~~
namespace eval ::loon::stars {
	proc bind {widget condition command} ${::loon::helper::bind}
}
~~~

The bind widget stores its bindings stored in a dictionary at
`State(${widget}.bindings)`.

These bindings are used by the helper `configure` procedure body

~~~
namespace eval ::loon::stars {
	variable validArgs
	set validArgs [list data sequence axisDirections]
	proc configure {widget args} ${::loon::helper::configure}
}
~~~

The `validArgs` variable is important for the `configure` procedure to
know which are valid arguments. For stars object we accept `-data`,
`-sequence`, and `-axisDirections`. The `validArgs` determines also
the evaluation order of the arguments. For each valid argument name,
the `configure` procedure expects a `isvalid_${name}` procedure which
either throws an error, or returns the argument value as it would be
stored in `State(${widget}.${name})`. `configure` also expects a
`set_${name}` procedure that returns `TRUE` or `FALSE` regarding
whether the argument has the stars state modified or not. So,

~~~
namespace eval ::loon::stars {
	isvalid_data {widget data} {
		
	}
	set_data {widget data} {

	}
}
~~~










We have added a `configure` subcommand, among others. In order to bind
code to `configure` e

which creates the object creator procedure ::loon::stars. The
procedure body can be modified if necessary.


The objects procedure requires to have a functions

~~~
::loon::stars::Init
::loon::stars::commands
~~~

defined. An API to the object must be provided by the object
designer.

We provide the 'configure' and 'bind' procedures that can be used
and automatically provide bindings! They are, in particular,
designed for widgets but also work with objects.

The 'bind' procedure can be specified as follows 

~~~
namespace eval loon::stars {
  proc bind {widget condition command} ${::loon::framework::bind}
}
~~~


The bind command will create a dict with the condition command
entries at State(${widget}.bindings) if it does not exist already.





