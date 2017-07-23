#!/bin/bash
pulp --watch --before clear build -t static/main.js &
node-sass --watch src/style/main.scss --output static/
