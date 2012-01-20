#!/bin/bash

# The Build The Update Needs To Be Applied In.
# (MAX LENGTH = 10)
export DELTA_BUILD=
# The DR # Or Some Other Unique Identifier.
# (MAX LENGTH = 20)
export DELTA_ID=
# A Short Description About The Changes That Were Made.
# (MAX LENGTH = 255)
export DELTA_DESC=

# [OPTIONAL] The User That Should Be Used To Run The Script.
# The Update Manager Will "su" To Become The User If
# Necessary.
export DELTA_RUN_USER=

# Insert The Logic For The Update Here. Perform Checks To
# Ensure That The Update Commands Are Successful. Return "0"
# At The End If The Update Succeeds Or "1" If Any Part Of
# The Update Fails.
function runUpdate()
{
}
