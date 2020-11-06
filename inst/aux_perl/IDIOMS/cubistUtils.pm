package IDIOMS::cubistUtils;
use strict;
use utf8;
use vars qw( @ISA @EXPORT $ROBUST_ERROR_FACTOR $FINAL_ERROR_INFLATOR $PLUSMINUS $UNITS
                          $DECIMAL_OUTPUT );
use Exporter;
@ISA    = qw( Exporter );
@EXPORT = qw( $ROBUST_ERROR_FACTOR $FINAL_ERROR_INFLATOR $PLUSMINUS $UNITS $DECIMAL_OUTPUT
              median mean_and_stdev weighted_mean_mean_error commify 
              getBranches makeNuts removeInfinites robustAdjustment
              inspectResults makeResults stringResults crackBranch crackNut XrowToHashRef
            );

$ROBUST_ERROR_FACTOR  = 5; # used by &robustAdjustment()
$FINAL_ERROR_INFLATOR = 1;
$UNITS = "ft"; # feet
$DECIMAL_OUTPUT = 2;

# Unicode Character “±” (U+00B1)
$PLUSMINUS = "\x{00B1}";#"±";
#print "$pm\n";  # need binmode STDOUT, ":encoding(utf8)"; near top of parent script.
#https://en.wikibooks.org/wiki/Perl_Programming/Unicode_UTF-8


# yes the next four functions are trivial, WHA is gambling that an abstraction layer 
# of sometype will eventually be needed, however we have constructed a verb with each
# of the functions that helps shout out what is purpose is
sub formRuleBranches {
    return(@_);
}

sub formBranches {
    return(@_);
}

sub formNuts {
    return(@_);
}

sub formSampleInfo {
    return(@_);
}

sub commify {
  my $text = reverse shift;
     $text =~ s/(\d\d\d)(?=\d)(?!\d*\.)/$1,/g;
  return scalar reverse $text;
}

sub XrowToHashRef {
  my @line   = @{shift(@_)}; # can not use @{shift} as it busts Perl's tokenizer
  my @labels = @{shift(@_)}; # so similar logic seen WHEN needed hereinafter
  my $href   = { };
  foreach my $i (0..$#line) { $href->{$labels[$i]} = $line[$i]; }
  $href;
}

sub crackBranch {
  my ($href, $w, $x) = ( shift, shift, shift );
  my @v = ( );
  foreach my $key (@$w) { #print "key=$key\n";
    push(@v, $href->{$key}($x));
  }
  @v;
}

sub crackNut {
  my ($href, $w, $k) = ( shift, shift, shift );
  my @v = ( );
  foreach my $key (@$w) { #print "key=$key\n";
    push(@v, $href->{$key}()->{$k});
  }
  @v;
}

sub makeResults {
  my @vals = @{shift(@_)};
  my @sups = @{shift(@_)};
  my @mues = @{shift(@_)};
  my @mins = @{shift(@_)};
  my @maxs = @{shift(@_)};
  my @errs = @{shift(@_)};
  my @mods = @{shift(@_)};

  my $diagnostics = shift;
  my $n_original = $diagnostics->{number_rules_orginally_found};
  my $n_finite   = $diagnostics->{number_rules_after_finite_test};

  my $fmt = "%.".$DECIMAL_OUTPUT."f";

  my $r = scalar @vals;
  my @musd = mean_and_stdev(\@sups);
  my $s = commify( int $musd[0] );

  @musd = mean_and_stdev(\@mins);
  my $min = sprintf($fmt, $musd[0]);
  @musd = mean_and_stdev(\@maxs);
  my $max = sprintf($fmt, $musd[0]);

  @musd = mean_and_stdev(\@vals);
  my $mean_estimate      = sprintf($fmt, $musd[0]);
  my $stdev_of_estimates = sprintf($fmt, $musd[1]);

  @musd = weighted_mean_mean_error(\@vals, \@errs);
  my $weighted_mean_estimate = sprintf($fmt, $musd[0]);
  my $weighted_mean_error    = sprintf($fmt, $musd[1]);

  my $href = {};
  $href->{number_rules_orginally_found}   = $n_original;
  $href->{number_rules_after_finite_test} = $n_finite;
  $href->{number_rules_after_robust_adj}  = $r;
  $href->{robust_error_factor}            = $ROBUST_ERROR_FACTOR;
  $href->{average_number_values_per_rule} = $s;
  $href->{minimum_of_minimums}    = $min;
  $href->{maximum_of_maximums}    = $max;
  $href->{mean_estimate}          = $mean_estimate;
  $href->{stdev_of_estimates}     = $stdev_of_estimates;
  $href->{weighted_mean_estimate} = $weighted_mean_estimate;
  $href->{weighted_mean_error}    = $weighted_mean_error;
  $href;
}


sub stringResults {
  my $href = shift;
  
  my $n_original = $href->{number_rules_orginally_found};
  my $n_finite   = $href->{number_rules_after_finite_test};
  
  my $fmt = "%.".$DECIMAL_OUTPUT."f";
  my $txt = "";

  $txt .= "   Some $n_original rules(regressions) were found, and $n_finite had finite output. Then the \n";

  my $r = $href->{number_rules_after_robust_adj};
  my $s = $href->{average_number_values_per_rule};
  my $error_factor = $href->{robust_error_factor};
  $txt .= "   robust check about median estimate and median error using a\n";
  $txt .= "   ROBUST_ERROR_FACTOR equaling $error_factor on the median error, provided $r rules\n";
  $txt .= "   with an average of $s water levels per rule used in final prediction.\n";

  my $min = $href->{minimum_of_minimums};
  my $max = $href->{maximum_of_maximums};
  $txt .= "   Mean of $r minimum and maximums, respectively, are $min $UNITS and $max $UNITS.\n";

  my $mean_estimate      = $href->{mean_estimate};
  my $stdev_of_estimates = $href->{stdev_of_estimates};
  $txt .= "   Mean and std.dev. of the $r rules, respectively, ".
                  "are $mean_estimate $UNITS and $stdev_of_estimates $UNITS.\n";

  my $weighted_mean_estimate = $href->{weighted_mean_estimate};
  my $weighted_mean_error    = $href->{weighted_mean_error};
  $txt .= "   Final weighted mean $PLUSMINUS weighted mean error is ".
                  "$weighted_mean_estimate $UNITS $PLUSMINUS $weighted_mean_error $UNITS.\n";
}

sub inspectResults {
  my @vals = @{shift(@_)};
  my @sups = @{shift(@_)}; # these are integers
  my @mues = @{shift(@_)};
  my @mins = @{shift(@_)};
  my @maxs = @{shift(@_)};
  my @errs = @{shift(@_)};
  my @mods = @{shift(@_)};

  my $fmt = "%.".$DECIMAL_OUTPUT."f";
  my $txt = "";

  map { $_ = sprintf($fmt, $_) } @vals;
  map { $_ = sprintf($fmt, $_) } @mues;
  map { $_ = sprintf($fmt, $_) } @mins;
  map { $_ = sprintf($fmt, $_) } @maxs;
  map { $_ = sprintf($fmt, $_) } @errs;

  $txt .= "COUNT: ". scalar     @vals ."\n";
  $txt .= "  VALS: ".join(", ", @vals)."\n";
  $txt .= "  SUPS: ".join(", ", @sups)."\n";
  $txt .= "  MUES: ".join(", ", @mues)."\n";
  $txt .= "  MINS: ".join(", ", @mins)."\n";
  $txt .= "  MAXS: ".join(", ", @maxs)."\n";
  $txt .= "  ERRS: ".join(", ", @errs)."\n";
  $txt .= "  MODS: ".join(", ", @mods)."\n";
}

sub removeInfinites {
  my @values   = @{shift(@_)};
  my @supports = @{shift(@_)};
  my @means    = @{shift(@_)};
  my @minimums = @{shift(@_)};
  my @maximums = @{shift(@_)};
  my @errors   = @{shift(@_)};
  my @models   = @{shift(@_)};

  my @vals = ( );
  my @sups = ( );
  my @mues = ( );
  my @mins = ( );
  my @maxs = ( );
  my @errs = ( );
  my @mods = ( );

  my ($inf, $nan) = (0,0);
  foreach my $i (0..$#values) {
    if($values[$i] =~ /NaN/ |   # can Perl actually result in nan and NaN
       $values[$i] =~ /nan/) {  # as "NaN" differences in underlying C? 
      $nan++;                   # The "or" her is a kind of a late fix in code development cycle
      $values[$i] = "inf";
    }
    if($values[$i] =~ /inf/) {
      $inf++;
      next 
    }
    push(@vals,   $values[$i]);
    push(@sups, $supports[$i]);
    push(@mues,    $means[$i]);
    push(@mins, $minimums[$i]);
    push(@maxs, $maximums[$i]);
    push(@errs,   $errors[$i]);
    push(@mods,   $models[$i]);
  }
  my $href = { };
  $href->{values}   = \@vals;
  $href->{supports} = \@sups;
  $href->{means}    = \@mues;
  $href->{mins}     = \@mins;
  $href->{maxs}     = \@maxs;
  $href->{errors}   = \@errs;
  $href->{models}   = \@mods;
  $href->{number_values_computed_as_infinite} = $inf;
  $href->{number_values_computed_as_nan}      = $nan;
  $href->{number_rules_after_finite_test} = scalar @vals;
  $href;
}


sub robustAdjustment {
  my @values   = @{shift(@_)};
  my @supports = @{shift(@_)};
  my @means    = @{shift(@_)};
  my @minimums = @{shift(@_)};
  my @maximums = @{shift(@_)};
  my @errors   = @{shift(@_)};
  my @models   = @{shift(@_)};

  my $medest = median(\@values);
  my $mederr = median(\@errors);
  #print STDERR "medest=$medest and mederr=$mederr\n";
  my @vals = ( );
  my @sups = ( );
  my @mues = ( );
  my @mins = ( );
  my @maxs = ( );
  my @errs = ( );
  my @mods = ( );

  foreach my $i (0..$#values) {
    #print STDERR "   low=",$medest - $ROBUST_ERROR_FACTOR*$mederr,"\n";
    #print STDERR $values[$i],"\n";
    #print STDERR "   hi=",$medest + $ROBUST_ERROR_FACTOR*$mederr,"\n\n";
    next if($medest - $ROBUST_ERROR_FACTOR*$mederr > $values[$i]);
    next if($medest + $ROBUST_ERROR_FACTOR*$mederr < $values[$i]);
    push(@vals,   $values[$i]);
    push(@sups, $supports[$i]);
    push(@mues,    $means[$i]);
    push(@mins, $minimums[$i]);
    push(@maxs, $maximums[$i]);
    push(@errs,   $errors[$i]);
    push(@mods,   $models[$i]);
  }

  my $href = { };
  $href->{used_medians_as_backup} = 0;
  $href->{number_rules_after_robust_adj} = scalar @values;
  if(scalar @vals == 0) { # abandoning the robustAdjustment() 
    push(@vals, median(\@values  )); # one cause could be a robust error factor that
    push(@sups, median(\@supports)); # is too small or pathological issues for one of the
    push(@mues, median(\@means   )); # predictions, so we abandon and set all the slots
    push(@mins, median(\@minimums)); # to the medians
    push(@maxs, median(\@maximums));
    push(@errs, median(\@errors  ));
    push(@mods, "median value and error used in robustAdjustment() because it ".
                "otherwise would come back with no values");
    $href->{used_medians_as_backup} = 1;
    $href->{number_rules_after_robust_adj} = 0;
  }

  $href->{values}   = \@vals;
  $href->{supports} = \@sups;
  $href->{means}    = \@mues;
  $href->{mins}     = \@mins;
  $href->{maxs}     = \@maxs;
  $href->{errors}   = \@errs;
  $href->{models}   = \@mods;
  $href;
}

sub median {
  my $values = shift;
  return("inf") if(scalar @$values == 0);
  my $mid = int @$values/2;
  my @sorted_values = sort { $a <=> $b } @$values;
  (@$values % 2) ? $sorted_values[ $mid ] : 
                  ($sorted_values[$mid-1] + $sorted_values[$mid])/2;
}

sub mean_and_stdev {
  my $values = shift;
  my ($sum, $sqsum) = (0, 0);
  foreach (@$values) {
    $sum   +=   $_;
    $sqsum += ( $_ ** 2 );
  }
  my $n = scalar @$values;
  return("inf", "inf") if($n == 0);
  my $mean = $sum / $n;
  $sqsum /= $n;
  $sqsum -= ( $mean ** 2 );
  $sqsum = 0 if($sqsum < 0); # insurance policy on small numbers getting involved
  ($mean,  sqrt $sqsum);
}

sub weighted_mean_mean_error {
  my @values  = @{shift(@_)};
  my @errors  = @{shift(@_)};

  return(("inf", "inf")) if(scalar @values == 0);

  my @weights = @errors;
  my ($sum1, $sum2) = (0, 0);
  foreach (@weights) { $sum1 += $_;    } # need sum so that we can ensure that the
  foreach (@weights) {    $_ /= $sum1; } # sum of the weight factors is unity
  $sum1 = 0;
  foreach my $i (0..$#values) {
    $sum1 += $values[$i] * $weights[$i];
    $sum2 += $errors[$i] * $weights[$i];
  }
  ($sum1, $sum2); # mean mean and mean error, respectively
}

sub getBranches {
  my ($href, $x) = ( shift, shift );
  my @which = ( );
  foreach my $key (keys %$href) {
    my $which = $href->{$key}($x);
    push(@which, $which) if(defined $which);
  }
  @which = reverse sort @which;
  my $k = undef;
  foreach my $i (0..$#which) { last unless($which[$i]); $k++; }
  [@which[0..($k-1)]];
}

sub makeNuts {
  my @bra = @{shift(@_)}; # need to deference to make a real copy 
  [ map { $_ .= "_NUT" } @bra];
}

1;
