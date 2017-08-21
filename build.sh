#!/bin/bash
bower install
pulp build --to static/main.js
node-sass src/style/main.scss > static/main.css
