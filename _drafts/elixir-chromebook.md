# Installing Elixir on Asus C101 Chromebook

## Install Ubuntu chroot in Termux

Follow the instructions at https://github.com/Neo-Oli/termux-ubuntu

## Prerequisites

    apt install wget gnupg

## Erlang

**THIS DOESN'T WORK**

    wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
    dpkg -i erlang-solutions_1.0_all.deb

    apt install erlang
    apt install elixir
