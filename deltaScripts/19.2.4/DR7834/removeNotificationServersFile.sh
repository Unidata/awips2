#
# Removes replication configuration files.
#
echo "DR #7834: removing notificationServers.xml file"

rm -rf /awips2/edex/data/utility/common_static/site/*/ebxml/registry/notificationServers.xml*

echo "DR #7834: successfully removed notificationServers.xml file"
