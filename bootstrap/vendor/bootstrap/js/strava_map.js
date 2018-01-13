
var ridesPath = {
"color": "#00b2ee",
"weight": 2,
"opacity": 0.4
};
var runsPath = {
"color": "#bcee68",
"weight": 2,
"opacity": 0.4
};

var map = new L.Map("map").setView([34.0195,-118.4912], 12);

map.addLayer(new L.TileLayer("https://cartodb-basemaps-{s}.global.ssl.fastly.net/dark_all/{z}/{x}/{y}.png"));

d3.json("data/rides_mls.json", function(error, collection) {
  if (error) throw error;

 map.addLayer(new L.geoJSON(collection, {style : ridesPath}));
});
d3.json("data/runs_mls.json", function(error, collection) {
  if (error) throw error;

 map.addLayer(new L.geoJSON(collection, {style : runsPath}));
});
