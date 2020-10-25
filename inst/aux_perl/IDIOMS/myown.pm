package IDIOMS::myown;
use strict;
use utf8;
use vars qw( @ISA @EXPORT );
use Exporter;
@ISA    = qw( Exporter );
@EXPORT = qw( countExtract_Well_and_Level useExtracted_Well_and_Level );
              
sub countExtract_Well_and_Level {
  my @sample_info = @_;
  my (@well_counts, @levl_counts, @parent_counts) = ( (), (), () );
  my @info = (); # outside foreach lexical scope deliberate
  foreach (@sample_info) {
    @info = split(/:/, $_);
    my @w = split(/=/, $info[0]); my $w = $w[1];
    my @l = split(/=/, $info[1]); my $l = $l[1];
    push(@well_counts,   $w);
    push(@levl_counts,   $l);
    if(defined $info[2]) {
      my @p = split(/=/, $info[2]);
      push(@parent_counts, $p[1]);
    }
  }
  if(scalar @info == 2) { @parent_counts = @well_counts; }
  [ \@well_counts, \@levl_counts, \@parent_counts ];
}

sub useExtracted_Well_and_Level {
  my @sample_info = @{shift(@_)};
  my $sample_txt = "";
  my ($total_wells, $total_levels) = ();
  foreach my $i (0..(scalar @{$sample_info[0]} - 1)) {
    my ($w, $l) = ($sample_info[0]->[$i], $sample_info[1]->[$i]);
    $sample_txt .= "   Cubist submodel ".($i+1)." had ".commify($w).
                                         " wells with ".commify($l)." levels.\n";
    $total_wells += $w; $total_levels += $l;
  }
  if(scalar @sample_info == 3) { $total_wells = $sample_info[2]->[0]; }
  $sample_txt .= "   'Total modeling' had ". commify($total_wells ).
                 " wells with a total of ".  commify($total_levels).
                 " water levels.\n";
}

1;
