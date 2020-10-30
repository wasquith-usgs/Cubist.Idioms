package IDIOMS::Aux;
use strict;
use utf8;
use vars qw( @ISA @EXPORT );
use Exporter;
@ISA    = qw( Exporter );
@EXPORT = qw( countExtract_Location_and_Response useExtracted_Location_and_Response );

use IDIOMS::cubistUtils;

sub countExtract_Location_and_Response {
  my @sample_info = @_;
  my (@loca_counts, @resp_counts, @parent_counts) = ( (), (), () );
  my @info = (); # outside foreach lexical scope deliberate
  foreach (@sample_info) {
    @info = split(/:/, $_);
    my @w = split(/=/, $info[0]); my $w = $w[1];
    my @l = split(/=/, $info[1]); my $l = $l[1];
    push(@loca_counts,   $w);
    push(@resp_counts,   $l);
    if(defined $info[2]) {
      my @p = split(/=/, $info[2]);
      push(@parent_counts, $p[1]);
    }
  }
  if(scalar @info == 2) { @parent_counts = @loca_counts; }
  [ \@loca_counts, \@resp_counts, \@parent_counts ];
}

sub useExtracted_Location_and_Response {
  my @sample_info = @{shift(@_)};
  my $sample_txt = "";
  my ($total_locas, $total_resps) = ();
  foreach my $i (0..(scalar @{$sample_info[0]} - 1)) {
    my ($w, $l) = ($sample_info[0]->[$i], $sample_info[1]->[$i]);
    $sample_txt .= "   Cubist submodel ".($i+1)." had ".commify($w).
                                     " locations with ".commify($l)." responses.\n";
    $total_locas += $w; $total_resps += $l;
  }
  if(scalar @sample_info == 3) { $total_locas = $sample_info[2]->[0]; }
  $sample_txt .= "   'Total modeling' had ".   commify($total_locas).
                 " locations with a total of ".commify($total_resps).
                 " responses.\n";
}

1;
