#!/bin/bash

psql -h dx1 -U awips -d metadata -f updateActiveTableKey.sql
