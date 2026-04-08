#!/usr/bin/env perl
# Conway's Game of Life - Terminal Edition
# A classic cellular automaton simulation in Perl
# Usage: perl game_of_life.pl [width] [height] [generations] [delay_ms]

use strict;
use warnings;
use Time::HiRes qw(usleep);

# Configuration
my $WIDTH       = $ARGV[0] // 40;
my $HEIGHT      = $ARGV[1] // 20;
my $GENERATIONS = $ARGV[2] // 200;
my $DELAY_US    = ($ARGV[3] // 100) * 1000;  # Convert ms to microseconds

my $ALIVE = "\x{2588}";  # Unicode full block
my $DEAD  = " ";

# Initialize grid with random cells (~30% alive)
sub init_grid {
    my @grid;
    for my $y (0 .. $HEIGHT - 1) {
        for my $x (0 .. $WIDTH - 1) {
            $grid[$y][$x] = (rand() < 0.3) ? 1 : 0;
        }
    }
    return \@grid;
}

# Seed famous patterns onto the grid
sub seed_patterns {
    my ($grid) = @_;

    # Glider at top-left
    my @glider = ([0,1], [1,2], [2,0], [2,1], [2,2]);
    for my $cell (@glider) {
        my ($dy, $dx) = @$cell;
        $grid->[$dy + 2][$dx + 2] = 1 if $dy + 2 < $HEIGHT && $dx + 2 < $WIDTH;
    }

    # R-pentomino near center (chaotic and long-lived)
    my $cx = int($WIDTH / 2);
    my $cy = int($HEIGHT / 2);
    my @rpent = ([0,1], [0,2], [1,0], [1,1], [2,1]);
    for my $cell (@rpent) {
        my ($dy, $dx) = @$cell;
        $grid->[$cy + $dy][$cx + $dx] = 1
            if $cy + $dy < $HEIGHT && $cx + $dx < $WIDTH;
    }
}

# Count live neighbors (toroidal wrapping)
sub count_neighbors {
    my ($grid, $y, $x) = @_;
    my $count = 0;
    for my $dy (-1, 0, 1) {
        for my $dx (-1, 0, 1) {
            next if $dy == 0 && $dx == 0;
            my $ny = ($y + $dy) % $HEIGHT;
            my $nx = ($x + $dx) % $WIDTH;
            $count += $grid->[$ny][$nx];
        }
    }
    return $count;
}

# Compute the next generation
sub next_generation {
    my ($grid) = @_;
    my @new_grid;
    for my $y (0 .. $HEIGHT - 1) {
        for my $x (0 .. $WIDTH - 1) {
            my $neighbors = count_neighbors($grid, $y, $x);
            my $alive = $grid->[$y][$x];
            if ($alive) {
                $new_grid[$y][$x] = ($neighbors == 2 || $neighbors == 3) ? 1 : 0;
            } else {
                $new_grid[$y][$x] = ($neighbors == 3) ? 1 : 0;
            }
        }
    }
    return \@new_grid;
}

# Count total live cells
sub population {
    my ($grid) = @_;
    my $count = 0;
    for my $y (0 .. $HEIGHT - 1) {
        for my $x (0 .. $WIDTH - 1) {
            $count += $grid->[$y][$x];
        }
    }
    return $count;
}

# Render the grid to terminal
sub display {
    my ($grid, $gen) = @_;
    print "\033[H\033[2J";  # Clear screen
    my $pop = population($grid);
    my $border = "+" . ("-" x $WIDTH) . "+";

    print "  Conway's Game of Life (Perl Edition)\n";
    print "  Gen: $gen | Population: $pop | Grid: ${WIDTH}x${HEIGHT}\n";
    print "  $border\n";

    for my $y (0 .. $HEIGHT - 1) {
        print "  |";
        for my $x (0 .. $WIDTH - 1) {
            print $grid->[$y][$x] ? $ALIVE : $DEAD;
        }
        print "|\n";
    }

    print "  $border\n";
    print "  Press Ctrl+C to quit\n";
}

# --- Main ---
binmode(STDOUT, ":utf8");
print "\033[?25l";  # Hide cursor

my $grid = init_grid();
seed_patterns($grid);

eval {
    for my $gen (1 .. $GENERATIONS) {
        display($grid, $gen);
        my $pop = population($grid);
        if ($pop == 0) {
            print "\n  All cells died at generation $gen. RIP.\n";
            last;
        }
        usleep($DELAY_US);
        $grid = next_generation($grid);
    }
};

print "\033[?25h";  # Restore cursor
print "\n  Simulation complete.\n";
