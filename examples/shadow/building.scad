union() {
    translate([0, 0, 0.1])
        cube([34.3, 34.3, 0.1], true);
    translate([0, 0, 0.3])
        cube([34.1, 34.1, 0.1], true);
    translate([0, 0, -0.21]) {
        first();
        translate([0, 0, 11])
            scale(2/3)
            second();
    }
}

module first() { difference () {
    translate([0, 0, 6])
        cube([33.9, 33.9, 11], true);
    translate([0, 0, 5])
        cube([32.1, 32.1, 10.1], true);
    per(16)
        union() {
                    circle(1, $fn=15);
                    translate([0, -1, 0])
                        scale ([1.5, 1, 1])
                        square(2);
                    translate([-5, -1, 0])
                        square(2);
        }
} }

module second() { difference () {
    translate([0, 0, 6])
        cube([33.9, 33.9, 11], true);
    translate([0, 0, 5])
        cube([32.1, 32.1, 10.1], true);
    per(16)
        union() {
                    translate([-4, -1, 0])
                        square(2);
        }
} }

module per(l) {
    for(r = [0 : 90 : 270]) {
        rotate([0, 0, r])
        translate([l, 0, 0])
        for(i = [- 15 : 5 : 15 ]) {
            rotate ([0, 90, 0])
                translate([-3, i, 0])
                linear_extrude(1)
                children();
        }
    }
}