========
ecureuil
========

.. image:: https://travis-ci.org/AntoineGagne/ecureuil.svg?branch=master
    :target: https://travis-ci.org/AntoineGagne/ecureuil

.. image:: http://img.shields.io/hexpm/v/ecureuil.svg?style=flat
    :target: https://hex.pm/packages/ecureuil

.. image:: https://img.shields.io/github/release/AntoineGagne/ecureuil?color=brightgreen
    :target: https://github.com/AntoineGagne/ecureuil/releases

.. image:: https://coveralls.io/repos/github/AntoineGagne/ecureuil/badge.svg?branch=master
    :target: https://coveralls.io/github/AntoineGagne/ecureuil?branch=master


:Author: `Antoine Gagné <gagnantoine@gmail.com>`_

.. contents::
    :backlinks: none

.. sectnum::

A library to parse and navigate HTML with CSS selectors.

Installation
============

This library is available on `hex.pm <https://hex.pm/packages/ecureuil>`_.
To install this library, simply add the following lines to your
``rebar.config``:

.. code-block:: erlang

    {ecureuil, "1.0.2"}

Development
===========

Running all the tests and linters
---------------------------------

You can run all the tests and linters with the ``rebar3`` alias:

.. code-block:: sh

    rebar3 check
