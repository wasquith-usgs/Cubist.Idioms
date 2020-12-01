#!/usr/bin/perl -w
use strict;

use utf8;
binmode STDOUT, ":encoding(utf8)";

# /usr/bin/perl: bad interpreter: Operation not permitted
# xattr -d com.apple.quarantine ./demoLOOP_C0.pl

use Cwd qw( abs_path );
use File::Basename qw( dirname );
use lib dirname(abs_path($0));

use IDIOMS::cubistUtils;
use IDIOMS::cubistAux;

use IDIOMS::cubistC0;

use vars qw(%OPTS);

use Getopt::Long;
my @options = qw( error_factor=f
                  string_results
                ); # these are the valid command line options
&GetOptions(\%OPTS, @options); # parse the command line options

$ROBUST_ERROR_FACTOR = $OPTS{"error_factor"} if($OPTS{"error_factor"});

my $show_string_results = $OPTS{"string_results"} ? 1 : 0;

my @RULE_BRA    = formRuleBranches($RULE_BRAC0);
my      @BRA    = formBranches($BRAC0);
my      @NUT    = formNuts($NUTC0);
my @SAMPLE_INFO = formSampleInfo($SAMPLE_STRC0);


sub readLabels {
  my @labels = ();
  while(<>) {
    next if(/^#/);
    s/\r\n$/\n/;
    chomp;
    @labels = split(/\t/, $_);
    last;
  }
  return(@labels);
}

sub readLine {
  my @labels = @{shift(@_)};
  $_ = shift;
  my %line = ();
  s/\r\n$/\n/;
  chomp;
  @line{@labels} = split(/\t/, $_);
  return( \%line );
}

if(! $show_string_results) {
  print "#ROW and rest of output names to insert here\n"
}

sub lineResults {
  my $href = shift;
  my $show_labels = shift;
  my @keys = sort keys %{$href};
  my @vals = ();
  foreach my $key (@keys) {
    push(@vals, $href->{$key});
  }
  print join("\t", @keys),"\n" if($show_labels);
  print join("\t", @vals),"\n";
}


if(! $show_string_results) {
  print "#ROW and rest of output names to insert here\n"
}

my $first = 1;
my @labels = readLabels(); #print STDERR "LABELS: @labels\n";
my $line = undef;
my $row_i = 0;
while(<>) {
  $row_i++;
  $line = readLine(\@labels, $_);
  # my %line = %{$line};
  # print STDERR "LINE: @line{@labels}\n";
  my $X = $line;
  my $diagnostics = {};
  my $checkin;

  my @which_bra = ();
  my @which_nut = ();
  my @vals = ();
  my @sups = ();
  my @mues = ();
  my @mins = ();
  my @maxs = ();
  my @errs = ();
  my @mods = ();

  foreach my $i (0..$#RULE_BRA) {
    $which_bra[$i] = getBranches($RULE_BRA[$i], $X);
    $which_nut[$i] = makeNuts($which_bra[$i]);
  }
  #print STDERR "@which_bra\n";
  #print STDERR "@which_nut\n";

  foreach my $i (0..$#BRA) {
    my $c = $i + 1;
    my @val = crackBranch($BRA[$i], $which_bra[$i],     $X   );
    my @bras = @{$which_bra[$i]};
    #foreach my $t (0..$#bras) {
    #   print "$bras[$t]  $val[$t]\n";
    #}
    #print "@val\n";
    map { $_ = "inf" if($_ eq "nan") } @val;
    #print "@val\n";
    push(@vals, @val);
    my @tmp = crackNut(   $NUT[$i], $which_nut[$i], "support");
    push(@mods, split(/,/, "$c," x scalar(@tmp)));
    # 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 3 3 3 etc
    push(@sups, @tmp);
    push(@sups, crackNut(   $NUT[$i], $which_nut[$i], "support"));
    push(@mues, crackNut(   $NUT[$i], $which_nut[$i],    "mean"));
    push(@mins, crackNut(   $NUT[$i], $which_nut[$i],     "min"));
    push(@maxs, crackNut(   $NUT[$i], $which_nut[$i],     "max"));
    push(@errs, crackNut(   $NUT[$i], $which_nut[$i],   "error"));
  }
  $diagnostics->{number_rules_orginally_found} = scalar @vals;
  if($diagnostics->{number_rules_orginally_found} == 0) {
    print STDERR "WARNING: row=$row_i : no rules original search\n";
  }

  #print STDERR "\n\n";
  #print STDERR "-------------------------------------\n";
  #$checkin = inspectResults(\@vals, \@sups, \@mues, \@mins, \@maxs, \@errs, \@mods);
  #print STDERR $checkin;

  my $clean = removeInfinites(\@vals, \@sups, \@mues, \@mins, \@maxs, \@errs, \@mods);
  @vals = @{$clean->{values}};
  @sups = @{$clean->{supports}};
  @mues = @{$clean->{means}};
  @mins = @{$clean->{mins}};
  @maxs = @{$clean->{maxs}};
  @errs = @{$clean->{errors}};
  @mods = @{$clean->{models}};
  $diagnostics->{number_rules_after_finite_test} =
        $clean->{number_rules_after_finite_test};
  $diagnostics->{number_values_computed_as_infinite} = 
        $clean->{number_values_computed_as_infinite};
  $diagnostics->{number_values_computed_as_nan} =
        $clean->{number_values_computed_as_nan}; 
  if($diagnostics->{number_rules_after_finite_test} == 0) {
    print STDERR "WARNING: row=$row_i : no rules after finite test\n";
    next;
  }

  #print STDERR "-------------------------------------\n";
  #$checkin = inspectResults(\@vals, \@sups, \@mues, \@mins, \@maxs, \@errs, \@mods);
  #print STDERR $checkin;

  #print "\n\n@vals\n";
  $clean = robustAdjustment(\@vals, \@sups, \@mues, \@mins, \@maxs, \@errs, \@mods);
  $diagnostics->{number_rules_after_robust_adj} = $clean->{number_rules_after_robust_adj};
  $diagnostics->{used_medians_as_backup} = $clean->{used_medians_as_backup};
  if($diagnostics->{number_rules_after_robust_adj} == 0) {
    print STDERR "WARNING: row=$row_i : no rules after robust adjustment\n";
  } else {
    @vals = @{$clean->{values}};
    @sups = @{$clean->{supports}};
    @mues = @{$clean->{means}};
    @mins = @{$clean->{mins}};
    @maxs = @{$clean->{maxs}};
    @errs = @{$clean->{errors}};
    @mods = @{$clean->{models}};
  }
  #print STDERR "-------------------------------------\n";
  #$checkin = inspectResults(\@vals, \@sups, \@mues, \@mins, \@maxs, \@errs, \@mods);
  #print STDERR $checkin;

  #print "$row_i, $diagnostics->{number_rules_orginally_found}, ".
  #       "$diagnostics->{number_rules_after_finite_test}, ".
  #       "$diagnostics->{number_rules_after_robust_adj}\n"; next;

  my $sample_info = countExtract_Location_and_Response(@SAMPLE_INFO);
  my $sample_txt  = useExtracted_Location_and_Response($sample_info);
  my $results = makeResults(\@vals, \@sups, \@mues, \@mins, \@maxs, \@errs, \@mods, $diagnostics);

  if($show_string_results) {
    my $results_str = stringResults($results);
    print "ROW $row_i : Cubist Model Results\n";
    print "$sample_txt";
    print "   For the requested 'X' prediction, the total modeling reports the following:\n";
    print $results_str;
    print "-------------------------------------------------------------------------------\n";
  } else {
    lineResults($results, $first);
    $first = 0;
  }
}
