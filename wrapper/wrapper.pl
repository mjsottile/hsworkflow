#
# wrapper for accessing computations from outside Haskell.  This is intended
# to be called from Haskell, and is responsible for staging data into a
# sandbox, executing a program, and moving results from the sandbox somewhere
# else.  Very crude currently, but enough for demonstration purposes.
#
# matt@galois.com
#
#!/usr/bin/perl

# include File::Temp tempdir to generate sandbox
use File::Temp qw/ tempdir /;

##
## wrapper script
##
## calling convention:
##
##   wrapper.pl <executable> <path to data repository> 
##
## assumes inputs are provided to STDIN as:
##
##   inname: inpath
##
## assumes outputs are printed to STDOUT as:
##
##   outname: outpath, MD5SUM
##
## one per line for N outputs
##

if ($#ARGV != 1) {
    print "USAGE: wrapper.pl <executable name> <data repository path>\n\n";
    die;
}

# get args
$exe = $ARGV[0];
$datarepo = $ARGV[1];

$sandbox = tempdir( CLEANUP => 1 );

# start building key/value table of input names and input paths
%input = ();

# loop until we see a blank line.  there is probably a cleaner way to
# do this, but Matt has crude perl knowledge.
$keepgoing = 1;
while ($keepgoing == 1) {
    $_ = <STDIN>;
    s/\r//g;
    s/\n//g;

    if (length($_) < 1) { 
	$keepgoing = 0;
    } else {
	($inName,$inPath) = split(/:/);
	
	$inPath =~ s/^ //g;
	$input{$inName} = $inPath;
    }
}

# go into the sandbox
chdir $sandbox || die "Can't use sandbox [$sandbox]\n";

# assemble the command line
$cmd = $exe." ";
foreach $key (keys(%input)) {
    $cmd .= "-$key $input{$key} ";
}
$cmd .= "-o $sandbox";

# go!
system($cmd) == 0
    || die "Error executing command for $exe\n";

# scan over sandbox to find files that came out
opendir(DIR,$sandbox) || die "Can't open sandbox [$sandbox]\n";
@files = grep { ! /^\./ && -f "$sandbox/$_" } readdir(DIR);
closedir(DIR);

# read in data repo for unique fname generation
opendir(DIR,$datarepo) || die "Can't open data repository [$datarepo]\n";
@repofiles = readdir(DIR);
closedir(DIR);
foreach $rfile (@repofiles) {
    $repotable{$rfile} = 1;
}

# figure out our OS since different machines have different built in
# md5 computation programs
$operating_system = `uname -s`;
$operating_system =~ s/\n//g;
$md5cmd = "";

# OSX
if ($operating_system eq "Darwin") {
    $md5cmd = "md5";
# Linux
} elsif ($operating_system eq "Linux") {
    $md5cmd = "md5sum";
} else {
# Get a real OS man
    print "Unsupported platform for MD5 computation!\n";
}

$counter = 1;

# spin through output files, computing MD5 sum and printing output from
# wrapper
foreach $file (@files) {
    ($outname, @junk) = split(/\./,$file);

    $md5 = `$md5cmd $file`;
    $md5 =~ s/\n//g;
    if ($operating_system eq "Darwin") {
	($junk,$md5) = split(/=/,$md5);
	$md5 =~ s/^ //g;
    } elsif ($operating_system eq "Linux") {
	($md5,$junk) = split(/ /,$md5);
    } else {
	print "Unsupported MD5 operating system!\n";
    }

    # generate repo filename
    while ($repotable{"$counter.dat"} == 1) {
	$counter++;
    }

    system ("mv $sandbox/$file $datarepo/$counter.dat");

    print "$outname=$datarepo/$counter.dat, $md5\n";
}


