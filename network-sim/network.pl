#!/usr/bin/env perl

use strict;
use warnings;

use local::lib 'extlib';

use Math::Random qw(random_poisson);

sub worker {
    my ($input) = @_;
    my $current_task;
    return sub {
        if ($current_task) {
            $current_task -= 1;
        } else {
            $current_task = $input->();
        }
    };
}

my $max_arrival_rate = 100;
sub event_source {
    my ($output, $arrival_rate, $mu) = @_;
    my $next_arrival_time = sub { scalar(random_poisson(1, $max_arrival_rate - $arrival_rate)); };
    my $time_since_last_send = $next_arrival_time->();
    return sub {
        if (++$time_since_last_send > $max_arrival_rate) {
            $output->(scalar(random_poisson(1, $mu)));
            $time_since_last_send = $next_arrival_time->();
        }
    };
}

sub queue {
    my ($max_length) = @_;
    my @q;
    my $dropped = 0;
    my $passed = 0;
    my $total_wait = 0;
    my $size = 0;
    my $length = 0;
    my $age = 0;
    return sub {
        if (@_) {
            if ($_[0] eq 'tick') {
                $length += @q;
                $age += 1;
                $_->[1] += 1 for (@q);
                return;
            }
            if ($_[0] eq 'stats') {
                return {
                    passed => $passed, 
                    dropped => $dropped, 
                    avg_wait => $total_wait/$passed, 
                    avg_size => $size/$passed,
                    avg_length => $length/$age
                };
            }
            if (@q >= $max_length) {
                $dropped += 1;
            } else {
                push @q, [$_[0], 0];
            }
        } else {
            my $event = shift @q;
            if ($event) {
                $passed += 1;
                $size += $event->[0];
                $total_wait += $event->[1];
                return $event->[0];
            }
        }
    };
}

my $q = queue(100);

my @clients = map { event_source($q, split /:/, $_) } @ARGV;

my @workers = (
    worker($q),
    worker($q),
    worker($q));

for (1..10000) {
    for my $node (@clients, @workers) {
        $node->();
    }
    $q->('tick');
}

my $stats = $q->('stats');

if ($ENV{VERBOSE}) {
    print "Packets passed: $stats->{passed}\nPackets dropped: $stats->{dropped}\n";
    print "Average queue wait: $stats->{avg_wait}\n";
    print "Average queue size: $stats->{avg_size}\n";
    print "Average queue length: $stats->{avg_length}\n";
} else {
    print "$stats->{passed}, $stats->{dropped}, $stats->{avg_wait}, $stats->{avg_size}, $stats->{avg_length}\n";
}
