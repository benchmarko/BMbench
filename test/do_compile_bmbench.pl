#!/usr/bin/perl -w
# do_compile_bmbench.pl - Compile bmbench
# (c) Marco Vieth, 2008
# http://www.benchmarko.de
#
# 22.04.2008 0.01  first test
#
#
$VERSION = '0.01';
use strict;
use Getopt::Std ();


sub get_osdir() {
  my $osname = $^O; #'aix', 'linux', 'hpux', 'solaris', 'MSWin32', ...('VMS') ($^O available since 5.004, not 5.001; MSWin32 for all Windows since 95)
  require Config;
  my $archname = $Config::Config{'archname'}; # see platform table in "perlport"
  return $osname .'/'. $archname;
}


sub create_dir($) {
  my($dir) = @_;
  require File::Path; # maybe multiple directories to create...
  File::Path::mkpath($dir, 0, 0775) || (warn("Cannot create directory '$dir': $!\n"), return);
}



my $g_bench1_r_unused = [
 ['awk', 'gawk', '$B_PRG --version', '', '$B_PRG -f ./$B_FILE.awk'],
 ['bash', 'bash', '$B_PRG --version', '', '$B_PRG -f ./$B_FILE.bash'],
 ['bwbasic', 'bwbasic', '', '', '$B_PRG ./$B_FILE.bas'],
 ['bc', 'bc', '', '', '$B_PRG ./$B_FILE.bc'], # set: export BC_LINE_LENGTH=80
 ['C (-O0)', 'gcc', '$B_PRG -v', '$B_PRG -Wall -Wtraditional -O0 $B_FILE.c -o ${B_FILE}_c_o0_${SYSNAME}', './${B_FILE}_c_o0_${SYSNAME}'],
 ['C (-O2)', 'gcc', '$B_PRG -v', '$B_PRG -Wall -Wtraditional -O2 $B_FILE.c -o ${B_FILE}_c_o2_${SYSNAME}', './${B_FILE}_c_o2_${SYSNAME}'],
 ['cobol', 'htcobol', '$B_PRG -V', '$B_PRG $B_FILE.cob -o ${B_FILE}_cob_${SYSNAME}', '$B_PRG ${B_FILE}_cob_${SYSNAME}'],
 ['forth', 'gforth', '$B_PRG -v', '', '$B_PRG ./$B_FILE.fs -e bye'],
 ['Fortran (g77 -O0)', 'g77', '$B_PRG -v', '$B_PRG -Wall -Wsurprising -Wunused -O0 ${B_FILE}.f -o ${B_FILE}_f_o0_${SYSNAME}', './${B_FILE}_f_o0_${SYSNAME}'],
 ['Fortran (g77 -O2)', 'g77', '$B_PRG -v', '$B_PRG -Wall -Wsurprising -Wunused -O2 ${B_FILE}.f -o ${B_FILE}_f_o2_${SYSNAME}', './${B_FILE}_f_o2_${SYSNAME}'],


 ['bash', 'bash', '$B_PRG --version', '', '$B_PRG -f ./$B_FILE.bash'],

 ];


sub do_run_bench1($$) {
  my($all_file, $rsp_file_r) = @_;

#if [ -d ./tmp ]; then
#  cd ./tmp
#fi

#SYSNAME=`uname -s`
# e.g. Linux

#echo $SYSNAME
#date

  #if ($::g_debug) { debug_msg("Using files '...'...", 0); }
  return 1;
}




my $g_bench1_r = [
  # ${INFILE}  : bmbench.${source}
  # ${OUTDIR}  : <osdir> (platform dependent)
  # ${OUTFILE} : ${OUTDIR}/${INFILE}  #TTT '.' => '_'?
  # $MOVE ??
  # PATH_SEPARATOR

  {
    'name' => 'boo',
    'source' => 'boo',
    'version' => {
      'program' => 'booc',
      'arguments' => '', # TTT '2>&1' only needed to take error message: 'Fatal error: No inputs specified.'
      'pattern' => 'version ([\d._]+)', # 'Boo Compiler version 0.7.8.2559 (CLR v2.0.50727.42)...'
    },
    'compile' => {
      'program' => 'booc',
      'arguments' => '-checked- -debug- -out:${OUTFILE}.exe ${INFILE}',
      #'$prg -checked- -debug- -out:$osdir/bmbench_boo_${prg}_${version}.exe bmbench.boo'
    },
    'execute' => {
      'program' => 'mono',
      'arguments' => '${OUTFILE}.exe',
    },
  },


  {
    'name' => 'Cobol (TinyCobol)',
    'source' => 'cob',
    'version' => {
      'program' => 'htcobol',
      'arguments' => '-V',
      'pattern' => '([\d._]+)', # 'TinyCOBOL - pre alpha 0.63.0 (linux-gnu 2005/07/24)...'
    },
    'compile' => {
      'preprocess' => 'export LD_LIBRARY_PATH=$HOME/usr/lib', #TTT
      'program' => 'htcobol',
      'arguments' => '-L$LD_LIBRARY_PATH -o ${OUTFILE} ${INFILE}',
    },
    'execute' => {
      'program' => '',
      'arguments' => '${OUTFILE}',
    },
  },


  {
    'name' => 'cs (mcs, mono)',
    'source' => 'cs',
    'version' => {
      'program' => 'mcs',
      'arguments' => '--version',
      'pattern' => 'version ([\d._]+)', # 'Mono C# compiler version 1.9.0.0'
    },
    'compile' => {
      'program' => 'mcs',
      'arguments' => '-optimize+ -warn:4 -out:${OUTFILE}.exe ${INFILE}',
      #'$prg -optimize+ -warn:4 -out:$osdir/bmbench_cs_${prg}_${version}.exe bmbench.cs'
    },
    'execute' => {
      'program' => 'mono',
      'arguments' => '${OUTFILE}.exe',
    },
  },


  {
    'name' => 'cs (csc, dotNet)',
    'source' => 'cs',
    'version' => {
      'program' => 'csc',
      'arguments' => '/help',
      'pattern' => 'version ([\d._]+)', # 'Microsoft (R) Visual C# 2005 Compiler version 8.00.50727.42...'
    },
    'compile' => {
      'program' => 'csc',
      'arguments' => '/optimize+ /warn:4 /out:${OUTFILE}.exe ${INFILE}',
      #'$prg -optimize+ -warn:4 -out:$osdir/bmbench_cs_${prg}_${version}.exe bmbench.cs'
    },
    'execute' => {
      'program' => '',
      'arguments' => '${OUTFILE}.exe',
    },
  },
  

  {
    'name' => 'C (gcc -O0)',
    'source' => 'c',
    'version' => {
      'program' => 'gcc',
      'arguments' => '-dumpversion', # or -v?
      'pattern' => '^([\d._]+)', # '4.2.1'
    },
    'compile' => {
      'program' => 'gcc',
      'arguments' => '-Wall -O0 -o ${OUTFILE}_O0 ${INFILE}',
      #'$prg -Wall -O0 bmbench.c -o $osdir/bmbench_c_${prg}_${version}_O0'
    },
    'execute' => {
      'program' => '',
      'arguments' => '${OUTFILE}_O0',
    },
  },


  {
    'name' => 'C (gcc -O2)',
    'source' => 'c',
    'version' => {
      'program' => 'gcc',
      'arguments' => '-dumpversion', # or -v?
      'pattern' => '^([\d._]+)', # '4.2.1'
    },
    'compile' => {
      'program' => 'gcc',
      'arguments' => '-Wall -O2 -o ${OUTFILE}_O2 ${INFILE}',
      #'$prg -Wall -O2 bmbench.c -o $osdir/bmbench_c_${prg}_${version}_O2'
    },
    'execute' => {
      'program' => '',
      'arguments' => '${OUTFILE}_O2',
    },
  },



  {
    'name' => 'C (cl -O0)',
    'source' => 'c',
    'version' => {
      'program' => 'cl',
      'arguments' => '/help',
      'pattern' => 'Version ([\d._]+)', # '12.00.8804'
    },
    'compile' => {
      'program' => 'cl',
      'arguments' => '/DUse_Windows /W4 /O0 /Fe${OUTFILE}_O0.exe /Fo${OUTFILE}_O0.obj ${INFILE}', #TTT: detect compiler instead of 'Use_Windows'? # ok: with or without .exe
    },
    'execute' => {
      'program' => '',
      'arguments' => '${OUTFILE}_O0.exe',
    },
  },
  

  {
    'name' => 'C (cl -O2)',
    'source' => 'c',
    'version' => {
      'program' => 'cl',
      'arguments' => '/help',
      'pattern' => 'Version ([\d._]+)', # '12.00.8804'
    },
    'compile' => {
      'program' => 'cl',
      'arguments' => '/DUse_Windows /W4 /O2 /Fe${OUTFILE}_O2.exe /Fo${OUTFILE}_O2.obj ${INFILE}', #TTT: detect compiler instead of 'Use_Windows'? # ok: with or without .exe
    },
    'execute' => {
      'program' => '',
      'arguments' => '${OUTFILE}_O2.exe',
    },
  },


  {
    'name' => 'C++ (g++ -O0)',
    'source' => 'cpp',
    'version' => {
      'program' => 'g++',
      'arguments' => '-dumpversion', # or -v?
      'pattern' => '^([\d._]+)', # '4.2.1'
    },
    'compile' => {
      'program' => 'g++',
      'arguments' => '-Wall -O0 -o ${OUTFILE}_O0 ${INFILE}',
      #'$prg -Wall -O0 bmbench.cpp -o $osdir/bmbench_cpp_${prg2}_${version}_O0' # name: g++ -> gpp!
    },
    'execute' => {
      'program' => '',
      'arguments' => '${OUTFILE}_O0',
    },
  },


  {
    'name' => 'C++ (g++ -O2)',
    'source' => 'cpp',
    'version' => {
      'program' => 'g++',
      'arguments' => '-dumpversion', # or -v?
      'pattern' => '^([\d._]+)', # '4.2.1'
    },
    'compile' => {
      'program' => 'g++',
      'arguments' => '-Wall -O2 -o ${OUTFILE}_O2 ${INFILE}',
      #'$prg -Wall -O2 bmbench.cpp -o $osdir/bmbench_cpp_${prg2}_${version}_O2' # name: g++ -> gpp!
    },
    'execute' => {
      'program' => '',
      'arguments' => '${OUTFILE}_O2',
    },
  },


  {
    'name' => 'C++ (cl -O0)',
    'source' => 'cpp',
    'version' => {
      'program' => 'cl',
      'arguments' => '/help',
      'pattern' => 'Version ([\d._]+)', # '12.00.8804'
    },
    'compile' => {
      'program' => 'cl',
      'arguments' => '/DUse_Windows /W4 /O0 /Fe${OUTFILE}_O0.exe /Fo${OUTFILE}_O0.obj ${INFILE}', #TTT: detect compiler instead of 'Use_Windows'? # ok: with or without .exe
    },
    'execute' => {
      'program' => '',
      'arguments' => '${OUTFILE}_O0.exe',
    },
  },


  {
    'name' => 'C++ (cl -O2)',
    'source' => 'cpp',
    'version' => {
      'program' => 'cl',
      'arguments' => '/help',
      'pattern' => 'Version ([\d._]+)', # '12.00.8804'
    },
    'compile' => {
      'program' => 'cl',
      'arguments' => '/DUse_Windows /W4 /O2 /Fe${OUTFILE}_O2.exe /Fo${OUTFILE}_O2.obj ${INFILE}', #TTT: detect compiler instead of 'Use_Windows'? # ok: with or without .exe
    },
    'execute' => {
      'program' => '',
      'arguments' => '${OUTFILE}_O2.exe',
    },
  },
  

  {
    'name' => 'javac',
    'source' => 'java',
    'version' => {
#      'program' => 'javac',
#      'arguments' => '-version 2>&1',
#      'pattern' => '^javac ([\d._]+)', # 'javac 1.5.0_15...' #TTT does not work?
       'program' => 'java',
       'arguments' => '-version',
       'pattern' => '^java version "([\d._]+)"', # 'java version "1.6.0_03"\nJava(TM) SE Runtime Environment (build 1.6.0_03-b05)\nJava HotSpot(TM) Client VM (build 1.6.0_03-b05, mixed mode, sharing)'
    },
    'compile' => {
      'program' => 'javac',
      'arguments' => '-O -d ${OUTDIR} ${INFILE}', #TTT
      #'$prg -O -d $osdir bmbench.java'
      'postprocess' => '${MOVE} ${OUTDIR}${PATH_SEPARATOR}bmbench.class ${OUTFILE}.class', #TTT
      #'mv $osdir/bmbench.class $osdir/bmbench_java_${prg}_${version}.class'
    },
    'execute' => {
      'program' => 'java',
      'arguments' => '${OUTFILE}',
    },
  },


  {
    'name' => 'js (DMD ds)',
    'source' => 'js',
    'version' => {
      'program' => 'ds',
      'arguments' => '-h', #
      'pattern' => 'DMDScript ([\d._]+)', # 'Digital Mars DMDScript 1.06...'
    },
#     'compile' => {
#       'preprocess' => '${COPY} ${INFILE} ${OUTFILE}.js',
#       'program' => 'ds',
#       'arguments' => 'xxx',
#     },
    'execute' => {
      'program' => 'ds',
      'arguments' => '${INFILE}',
    },
  },


  {
    'name' => 'js (NGS)',
    'source' => 'js',
    'version' => {
      'program' => 'js_ngs',
      'arguments' => '--version',
      'pattern' => '^NGS JavaScript Interpter ([\d._]+)', # 'NGS JavaScript Interpter 0.2.5...'
    },
    'compile' => {
      'preprocess' => '${COPY} ${INFILE} ${OUTFILE}.js',
      'program' => 'js_ngs',
      #'arguments' => '-c -O2 ${INFILE}',
      'arguments' => '-c -O2 ${OUTFILE}.js',
      #'js_ngs -c -O2 bmbench.js'
      #'postprocess' => '${MOVE} ${OUTDIR}${PATH_SEPARATOR}bmbench.jsc ${OUTFILE}.jsc', #TTT
      #'mv bmbench.jsc $osdir/bmbench_js_${prg}_${version}.jsc'
    },
    'execute' => {
      'program' => 'js_ngs',
      'arguments' => '${OUTFILE}.jsc',
    },
  },


  {
    'name' => 'Clisp (GNU clisp)',
    'source' => 'lisp',
    'version' => {
      'program' => 'clisp',
      'arguments' => '--version',
      'pattern' => '^GNU CLISP ([\d._]+)', # 'GNU CLISP 2.41 (2006-10-13)...'
    },
    'compile' => {
      'program' => 'clisp',
      'arguments' => '-L english -q -c ${INFILE} -o ${OUTFILE}.fas ', # oder of -c and -o is important!
    },
    'execute' => {
      'program' => 'clisp',
      'arguments' => '${OUTFILE}.fas',
    },
  },


  {
    'name' => 'Perl (CC_B)',
    'source' => 'pl',
    'version' => {
      'program' => 'perl',
      'arguments' => '-v', # or: $] : 5.008008
      'pattern' => 'This is perl, v([\d._]+)', # '\nThis is perl, v5.8.8\n'
    },
#     'compile' => {
#       'program' => 'perlcc',
#       'arguments' => '-B -o ${OUTFILE} ${INFILE}',
#       #'perlcc -B -o $osdir/bmbench_pl_${prg}_${version} bmbench.pl' # not for Perl 5.10 any more
#     },
    'execute' => {
      'program' => 'perl',
      'arguments' => '${INFILE}',
    },
  },

];




sub substitute_vars1($$) {
  my($para_r, $str) = @_;
  #my $val;
  #$str =~ s/(?<!\\)\${([^}]+)}/ (defined $para_r->{$1}) ? &substitute_vars1($para_r, $para_r->{$1}) : (defined Bm::get_env($1)) ? Bm::get_env($1) : ('${'. $1 .'}') /geo;
  #$str =~ s/(?<!\\)\${([^}]+)}/ (defined $para_r->{$1}) ? $para_r->{$1} : (defined($val = Bm::get_env($1))) ? $val : ('${'. $1 .'}') /geo;
  #$str =~ s/(?<!\\)\${([^}]+)}/ (defined $para_r->{$1}) ? $para_r->{$1} : ('${xxx'. $1 .'}') /geo;
  #$str =~ s/(?<!\\)\${([^}]+)}/ (defined $para_r->{$1}) ? &substitute_vars1($para_r, $para_r->{$1}) : ('${xxx'. $1 .'}') /geo;
  $str =~ s/(?<!#)\${([^}]+)}/ (defined $para_r->{$1}) ? &substitute_vars1($para_r, $para_r->{$1}) : ('${'. $1 .'}') /geo;
    # replace all $var not preceided by a '#'(not backslash) by its value (negative look-behind assertion)
    # replacement is done recursive, if a parameter from para_r is referenced... (with correct order we would not need this)
  #$str =~ s/\\(\$|")/$1/go;
    # replace '\$' by '$', '\"' by '"'; don't need this?
  return $str;
}



sub get_version1($) {
  my($version_r) = @_;
  my($program, $arguments, $pattern) = @$version_r{qw(program arguments pattern)};

  my $prg = join(' ', $program, $arguments, '2>&1'); # '2>&1' to catch error messages TTT
  #$prg = substitute_vars1($para_r, $prg); #usually not needed, but to be sure...
  print STDERR "DEBUG: get_version1: prg='$prg'\n";
  my $out = `$prg`;
  if (!$out) {
    return;
  }

  my ($version) = ($out =~ /$pattern/);
  #if ($version) {
  #  print STDERR "DEBUG: prg='$prg': found version='$version'\n";
  #}
  return $version;
}


sub compile1($$) {
  my($compile_r, $para_r) = @_;
  my($preprocess, $program, $arguments, $postprocess) = @$compile_r{qw(preprocess program arguments postprocess)};

  if ($preprocess) {
    my $prg = $preprocess;
    $prg = substitute_vars1($para_r, $prg);
    print STDERR "DEBUG: compile1: preprocess='$prg'\n";
    my $out = `$prg`;
    print "Compile preprocess '$prg': OUT='". ((defined $out) ? $out : 'undef') ."'\n";
  }

  if (!$program) {
    my $out = 'Nothing to compile.';
    print STDERR "DEBUG: Nothing to compile.\n";
    return $out;
  }

  my $prg = join(' ', $program, $arguments);
  $prg = substitute_vars1($para_r, $prg);
  print STDERR "DEBUG: compile1: prg='$prg'\n";
  my $out = `$prg`;
  #print "OUT($prg): $out\n";
  print "Compile '$prg': OUT='". ((defined $out) ? $out : 'undef') ."'\n";

  if ($postprocess) {
    my $prg = $postprocess;
    $prg = substitute_vars1($para_r, $prg);
    print STDERR "DEBUG: compile1: postprocess='$prg'\n";
    my $out = `$prg`;
    print "Compile postprocess '$prg': OUT='". ((defined $out) ? $out : 'undef') ."'\n";
  }

  return $out;
}


sub execute1($$) {
  my($execute_r, $para_r) = @_;
  my($program, $arguments) = @$execute_r{qw(program arguments)};

  my $prg = $arguments;
  if ($program) {
    $prg = join(' ', $program, $arguments);
  }
  $prg = substitute_vars1($para_r, $prg);

  print STDERR "DEBUG: execute: prg='$prg'\n";
  my $out = `$prg`;
  print "Execute '$prg': OUT=...\n". ((defined $out) ? $out : 'undef') ."\n";
  return $out;
}


sub bm_conv_fn {
  #my($fn) = @_;
  if ($^O eq 'MSWin32') { return join('\\', split(/\//, $_[0])); } else { return $_[0]; }
}


sub doit1($) {
  my($steps) = @_;

  my $osdir = get_osdir();
  #print STDERR "DEBUG: osdir='$osdir'\n";
  if (! -d $osdir) {
    print "Creating osdir='$osdir'...\n";
    create_dir($osdir) || return;
  } else {
    print "Using osdir='$osdir'.\n";
  }

  my $move = ($^O eq 'MSWin32') ? 'move' : 'mv'; # or use 'mv' also for Windows?
  my $copy = ($^O eq 'MSWin32') ? 'copy' : 'cp -p';

  my $path_separator = ($^O eq 'MSWin32') ? '\\' : '/';

  foreach my $entry_r (@$g_bench1_r) {
    my $name = $entry_r->{'name'};
    print STDERR "DEBUG: doit1: name='$name'\n";
    my $version_r = $entry_r->{'version'};

    my $version;
    if ($steps =~ /\bversion\b/) {
      $version = get_version1($version_r);
    }
    if ($version) {
      print STDERR "DEBUG: doit1: version='$version'\n";
      my $source = $entry_r->{'source'};

      my $compile_r = $entry_r->{'compile'};

      my $outfile;
      if ($compile_r) {
        my $program = $compile_r->{'program'}; #TTT

        my $prg2 = $program;
        $prg2 =~ tr/[A-Za-z_]/_/cs; # replace non word characters by 'x'
        $outfile = '${OUTDIR}/${INFILE}_'. $prg2 .'_'. $version;
      }

      my $para_r = {
        'SOURCE' => $source,
        'INFILE' => 'bmbench.${SOURCE}',  # bmbench.${source}
        'OUTDIR' => bm_conv_fn($osdir),
        'PATH_SEPARATOR' => $path_separator,
        'OUTFILE' => bm_conv_fn($outfile),
        'MOVE' => $move,
        'COPY' => $copy,
      };

      if ($steps =~ /\bcompile\b/) {
        my $out = compile1($compile_r, $para_r);
      }
      if ($steps =~ /\bexecute\b/) {
        if (1) { #TTT: compile ok?
          my $out = execute1($entry_r->{'execute'}, $para_r);
        }
      }
    }

  }
  return 1;
}




sub main() {
  my %opts = (
   's' => 'version,compile,execute',
  );
  if (!Getopt::Std::getopts("s:hd:", \%opts) or ($#ARGV < -1) or exists($opts{'h'})) {
    print STDERR "do_compile_bmbench.pl v$::VERSION -- do_compile_bmbench\n";
    require File::Basename;
    print STDERR "Usage: ". File::Basename::basename($0) ." [options]\n";
    #print STDERR "-a file  : set 'all' file (default: '$opts{'a'}')\n";
    print STDERR "-s steps : set steps 'version,compile,execute' (default: '$opts{'s'}')\n";
    print STDERR "-h       : help\n";
    print STDERR "-d level : set debug level\n";
    exit 1;
  }

  my $rc = doit1($opts{'s'});
  return (($rc) ? 1 : 0);
}

exit(main());

__END__



##########################################################

##########################################################



sub doit1_ok1() {
  my $osdir = get_osdir();
  #print STDERR "DEBUG: osdir='$osdir'\n";
  if (! -d $osdir) {
    print "Creating osdir='$osdir'...\n";
    create_dir($osdir) || return;
  } else {
    print "Using osdir='$osdir'.\n";
  }

  my $out;
  my $prg;
  my $version;

  #"C:\Program Files\Mono-1.2.6\bin\setmonopath.bat"
  # set PATH for booc, mcs on Windows

  #"C:\Program Files\Microsoft Visual Studio\VC98\Bin\VCVARS32.BAT"

  #"C:\Program Files\Microsoft Visual Studio .Net 2003\Common7\Tools\vsvars32.bat"
  #"C:\Program Files\Microsoft Visual Studio .Net 2003\Vc7\bin\vsvars32.bat"

  #"C:\Program Files\Microsoft Visual Studio 8\Common7\Tools\vsvars32.bat" for all!
  #"C:\Program Files\Microsoft Visual Studio 8\VC\bin\vcvars32.bat" ??

  #C:\winprg\mstudio\VC98\Bin\VCVARS32.BAT"

  #C:\winprg\dmdscript\ds.exe  # set PATH="%PATH%";c:\winprg\dmd\bin;c:\winprg\dm\bin

##


  $prg = 'booc';
  $out = `$prg`;
  #print $out;
#Boo Compiler version 0.7.8.2559 (CLR v2.0.50727.42)
#Fatal error: No inputs specified.
  ($version) = ($out =~ /version (\S+)/);
  if ($version) {
    $out = `$prg -checked- -debug- -out:$osdir/bmbench_boo_${prg}_${version}.exe bmbench.boo`;
    print "OUT($prg): $out\n";
  }

##

  $prg = 'mcs';
  $out = `$prg --version`;
#Mono C# compiler version 1.9.0.0
  ($version) = ($out =~ /version (\S+)/);
  if ($version) {
    $out = `$prg -optimize+ -warn:4 -out:$osdir/bmbench_cs_${prg}_${version}.exe bmbench.cs`;
# -optimize+ no effect?
    print "OUT($prg): $out\n";
  }


##

  $prg = 'gcc';
  $out = `$prg -dumpversion`;
#4.2.1
  ($version) = ($out =~ /^(\S+)/);
  if ($version) {
    $out = `$prg -Wall -O0 bmbench.c -o $osdir/bmbench_c_${prg}_${version}_O0`;
    print "OUT($prg): $out\n";

    $out = `$prg -Wall -O2 bmbench.c -o $osdir/bmbench_c_${prg}_${version}_O2`;
    print "OUT($prg): $out\n";
  }

##

  $prg = 'g++';
  $out = `$prg -dumpversion`;
#4.2.1
  ($version) = ($out =~ /^(\S+)/);
  if ($version) {
    my $prg2 = 'gpp'; #TTT
    $out = `$prg -Wall -O0 bmbench.cpp -o $osdir/bmbench_cpp_${prg2}_${version}_O0`;
    print "OUT($prg): $out\n";

    $out = `$prg -Wall -O2 bmbench.cpp -o $osdir/bmbench_cpp_${prg2}_${version}_O2`;
    print "OUT($prg): $out\n";
  }


##

  $prg = 'javac';
  $out = `$prg -version 2>&1`;
#javac 1.5.0_15
#javac: no source files
#...
  ($version) = ($out =~ /^javac (\S+)/);
  if ($version) {
    print STDERR "DEBUG: $prg: version='$version'\n";
    $out = `$prg -O -d $osdir bmbench.java`;
    print "OUT($prg): $out\n";

    $out = `mv $osdir/bmbench.class $osdir/bmbench_java_${prg}_${version}.class`;
    print "OUT($prg): $out\n";
  }


##

  $prg = 'js_ngs';
  $out = `$prg --version`;
#NGS JavaScript Interpter 0.2.5
#Copyright (C) 1998 New Generation Software (NGS) Oy.
#...
  ($version) = ($out =~ /^NGS JavaScript Interpter (\S+)/o);
  if ($version) {
    $out = `js_ngs -c -O2 bmbench.js`;
    print "OUT($prg): $out\n";

    $out = `mv bmbench.jsc $osdir/bmbench_js_${prg}_${version}.jsc`;
    print "OUT($prg): $out\n";
  }


##

  $prg = 'perlcc';

  #perl -v
  #  This is perl, v5.8.8
  $version = $]; #5.008008
  if ($version) {
    $out = `perlcc -B -o $osdir/bmbench_pl_${prg}_${version} bmbench.pl`;
    print "OUT($prg): $out\n";
  }

  return 1;
}



#source_shell1.pl

sub test_substitute_vars($$) {
  my($para_r, $str) = @_;
  #$str =~ s/(?<!\\)\${([^}]+)}/ (defined $para_r->{$1}) ? &test_substitute_vars($para_r, $para_r->{$1}) : (defined Bm::get_env($1)) ? Bm::get_env($1) : ('${'. $1 .'}') /geo;
  $str =~ s/(?<!\\)\${([^}]+)}/ (defined $para_r->{$1}) ? &test_substitute_vars($para_r, $para_r->{$1}) : ('${'. $1 .'}') /geo;
    # replace all $var not preceided by a backslash by its value (negative look-behind assertion)
    # replacement is done recursive, if a parameter from para_r is referenced... (with correct order we would not need this)
  #$str =~ s/\\(\$|")/$1/go;
    # replace '\$' by '$', '\"' by '"'; don't need this?
  return $str;
}



#stat_misc.pl

sub substitute_vars1($$) {
  my($para_r, $str) = @_;
  #my $val;
  #$str =~ s/(?<!\\)\${([^}]+)}/ (defined $para_r->{$1}) ? &substitute_vars1($para_r, $para_r->{$1}) : (defined Bm::get_env($1)) ? Bm::get_env($1) : ('${'. $1 .'}') /geo;
  #$str =~ s/(?<!\\)\${([^}]+)}/ (defined $para_r->{$1}) ? $para_r->{$1} : (defined($val = Bm::get_env($1))) ? $val : ('${'. $1 .'}') /geo;
  $str =~ s/(?<!\\)\${([^}]+)}/ (defined $para_r->{$1}) ? $para_r->{$1} : ('${'. $1 .'}') /geo;
    # replace all $var not preceided by a backslash by its value (negative look-behind assertion)
    # replacement is done recursive, if a parameter from para_r is referenced... (with correct order we would not need this)
  #$str =~ s/\\(\$|")/$1/go;
    # replace '\$' by '$', '\"' by '"'; don't need this?
  return $str;
}

