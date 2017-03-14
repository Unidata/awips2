#!/bin/bash

# locate EDEX_HOME
if [ "${EDEX_HOME}" = "" ]; then
	default_edex_home=$(cd $(dirname $0)/../../../../edex;pwd)
	if test -d $default_edex_home; then
		export EDEX_HOME=$default_edex_home
	else
		echo "Unable to locate EDEX_HOME"
		exit 1
	fi
fi

# source set_hydro_env since it sets up the environment for get_apps_defaults
# and contains exports for the database connection

. ${EDEX_HOME}/data/hdf5/hydroapps/set_hydro_env

# return the variable's value if it is defined, otherwise return the given value
get_default_value() { #(variable, value)
    if [ "$1" = "" ]; then
        echo $2
    else
        echo $1
    fi
}

export ADAPT_SITE_ID=$(get_apps_defaults "ADAPT_SITE_ID")
export FXA_LOCAL_TZ=$(get_apps_defaults "FXA_LOCAL_TZ")
export TZ=$(get_apps_defaults "TZ")

export FXA_TEXT=$(get_default_value $(get_apps_defaults "FXA_TEXT") "fxatext")

# setup climate specific environment variables
export DB_HM=$(get_default_value "$DB_HM" "hmdb")

export CLIMATE_DIR=$(get_default_value "$CLIMATE_DIR" "$(cd $(dirname $0)/../../;pwd)")
export CLIMATE_DATA_DIR=${CLIMATE_DIR}/data
export CLIMATE_TMP_DIR=${CLIMATE_DIR}/tmp
export CLIMATE_BIN_DIR=${CLIMATE_DIR}/bin/Linux

