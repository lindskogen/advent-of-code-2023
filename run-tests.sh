#!/usr/bin/env node

const { spawn } = require('child_process');

const fs = require('fs');

fs.readdirSync(".").forEach((file) => {
    if (!file.startsWith("day")) {
        return;
    }

    const child = spawn('roc', ["test"], {
        cwd: file
    });


    child.on('exit', function (code) {
        if (code !== 0) {
            console.log(`Test ${file} failed with code ${code}`);
        } else {
            console.log(`Test ${file} passed`);
        }
    });
})

