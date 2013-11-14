-record(islet_root, {
        path,
        directory = [],
        file = []
    }).

-record(islet, {
        system = [],
        interface = [],
        sandbox,
        chroot
    }).
