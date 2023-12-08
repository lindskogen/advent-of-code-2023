#!/usr/bin/env node

const { spawn } = require('child_process');

const fs = require('fs');

fs.readdirSync(".").forEach((file) => {
    if (!file.startsWith("day")) {
        return;
    }

    const child = spawn('roc', ["run", '--optimize'], {
        cwd: file
    });

    const start = new Date().getTime();
    let killed = false;

    const timeoutRef = setTimeout(() => {
        child.kill();
        killed = true;
        console.error(`${file} timed out`);
    }, 1000 * 10);

    

    child.on('exit', function (code) {
        clearTimeout(timeoutRef);
        if (!killed) {
            console.log(`Test ${file} ended with code ${code} for: ${new Date().getTime() - start}ms`);
        }
    });
})
