# Translink Bot
[![Build Status](https://travis-ci.org/ntyndall/translink.bot.svg?branch=master)](https://travis-ci.org/ntyndall/translink.bot)
[![codecov](https://codecov.io/gh/ntyndall/translink.bot/branch/master/graph/badge.svg)](https://codecov.io/gh/ntyndall/translink.bot)

This R package is an implementation of a slack bot using the translink live API for train timetables in Northern Ireland. This service utilises the `plumber` package in R and is running as a service on digital ocean. It is a work in progress, but for now, follow the installation instructions below to get up and running. 

## Installation 
...

## Usage
Install the bot `trains` to a channel and use the following commands
 - /ttrains ... to ...
 - /tset x ... to ...
 - /tget x
 - /tdelete ...
 - /tinfo x
 - /tinfo all
where `x` is a string e.g. home/work/shops. This string cannot take any of the following commands above.
