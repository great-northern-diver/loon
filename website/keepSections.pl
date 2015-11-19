#!/usr/bin/perl

## delete all files in generated
unlink glob "/md/generated/*.*";

# Define all languages
@lang = ("R", "Tcl", "Python");
$len = @lang;

#print "The total Number of arguments is: $#ARGV\n";

if ($#ARGV != 0) {
    print "Please provide which sections to keep.\n";
    exit;
}

# Delete element that matches argument
my $index = 0;
$index++ until ($lang[$index] eq $ARGV[0] || $index > $len);

if ($index > $len) {
    print "Ivalid argument: $ARGV[0].\n";
    exit;
}
splice(@lang, $index, 1);


#print "language(s) to delete are: @lang\n";

@sections = ("intro", "states", "linking", "layer", "bind",
	     "display_plot", "display_hist", "display_pairs", "display_graph", "display_serialaxes", "display_inspectors",
	     "layout");


foreach my $section (@sections) {
    ## Open file
    open(IN,"md/learn_$section.md") or die $!;
    
    ## Open new file
    open(OUT, ">md/generated/learn_$ARGV[0]_$section.md") or die $!;
    
    ## read entire file into string
    local $/;
    $doc = <IN>;
    
    ## Get rid of sections
    foreach $language (@lang) {
	$doc =~ s/<$language>.*?<\/$language>//msg;
	$doc =~ s/^~~~\{\.$language.*?^~~~//msig;
    }
    
    ## Now remove tags of Language
    $doc =~ s/<\/?$ARGV[0]>//g;


    ## Now remove multiple new lines
    $doc =~ s/[\n]{3,}/\n\n/mg;

    ##\{.*?\.$language.*?}

    print OUT $doc;

    close(IN);
    close(OUT);
    print "done with $ARGV[0] $section.\n"
}

