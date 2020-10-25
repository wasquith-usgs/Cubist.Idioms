#!/usr/bin/perl -w
use strict;

use utf8;
binmode STDOUT, ":encoding(utf8)";

use Cwd qw( abs_path );
use File::Basename qw( dirname );
use lib dirname(abs_path($0));

use vars qw(%OPTS);

use IDIOMS::myutils;
use IDIOMS::myown;

use IDIOMS::cubistC1;
use IDIOMS::cubistC2;
use IDIOMS::cubistC3;

use vars qw( %OPTS $ROW $X );

use Getopt::Long;
my @options = qw( error_factor=f
                  string_results
                ); # these are the valid command line options
&GetOptions(\%OPTS, @options); # parse the command line options

$ROBUST_ERROR_FACTOR = $OPTS{"error_factor"} if($OPTS{"error_factor"});

my $show_string_results = $OPTS{"string_results"} ? 1 : 0;

my @RULE_BRA = ($RULE_BRAC1, $RULE_BRAC2, $RULE_BRAC3);
my      @BRA = ($BRAC1, $BRAC2, $BRAC3);
my      @NUT = ($NUTC1, $NUTC2, $NUTC3);
my @SAMPLE_INFO = ($SAMPLE_STRC1, $SAMPLE_STRC2, $SAMPLE_STRC3);

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

$ROW = 0;
$X = { X => 1, pi => 3.141592 };

{
local $/ = undef;
my $f = <>;
eval { eval $f; };
}

foreach my $i (0..$#RULE_BRA) {
  $which_bra[$i] = getBranches($RULE_BRA[$i], $X);
  $which_nut[$i] = makeNuts($which_bra[$i]);
}
#print STDERR "@which_bra\n";
#print STDERR "@which_nut\n";

foreach my $i (0..$#BRA) {
  my $c = $i + 1;
  push(@vals, crackBranch($BRA[$i], $which_bra[$i],     $X   ));
  my @tmp = crackNut(   $NUT[$i], $which_nut[$i], "support");
  push(@mods, split(/,/, "$c," x scalar(@tmp)));
  # 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 3 3 3 etc
  push(@sups, @tmp);
  push(@mues, crackNut(   $NUT[$i], $which_nut[$i],    "mean"));
  push(@mins, crackNut(   $NUT[$i], $which_nut[$i],     "min"));
  push(@maxs, crackNut(   $NUT[$i], $which_nut[$i],     "max"));
  push(@errs, crackNut(   $NUT[$i], $which_nut[$i],   "error"));
}
$diagnostics->{number_rules_orginally_found} = scalar @vals;
if($diagnostics->{number_rules_orginally_found} == 0) {
  print STDERR "FATAL: no rules original search\n";
  exit;
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
  print STDERR "FATAL: no rules after finite test\n";
  exit;
}
#print STDERR "-------------------------------------\n";
#$checkin = inspectResults(\@vals, \@sups, \@mues, \@mins, \@maxs, \@errs, \@mods);
#print STDERR $checkin;

$clean = robustAdjustment(\@vals, \@sups, \@mues, \@mins, \@maxs, \@errs, \@mods);
$diagnostics->{number_rules_after_robust_adj} = $clean->{number_rules_after_robust_adj};
$diagnostics->{used_medians_as_backup} = $clean->{used_medians_as_backup};
if($diagnostics->{number_rules_after_robust_adj} == 0) {
  print STDERR "WARNING: no rules after robust adjustment\n";
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


my $sample_info = countExtract_Location_and_Response(@SAMPLE_INFO);
my $sample_txt  = useExtracted_Location_and_Response($sample_info);
my $results = makeResults(\@vals, \@sups, \@mues, \@mins, \@maxs, \@errs, \@mods, $diagnostics);

if($show_string_results) {
  my $results_str = stringResults($results);
  print "Cubist Model Results\n";
  print "$sample_txt";
  print "   For the requested 'X' prediction, the total modeling reports the following:\n";
  print $results_str;
  print "-------------------------------------------------------------------------------\n";
} else {
  print "$results->{weighted_mean_estimate},$results->{weighted_mean_errors}\n";
}
