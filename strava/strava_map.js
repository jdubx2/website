
var myStyle = {
"color": "#ff7800",
"weight": 1,
"opacity": 0.4
};

var map = new L.Map("map").setView([34.123601,-118.233569], 13);

map.addLayer(new L.TileLayer("https://cartodb-basemaps-{s}.global.ssl.fastly.net/dark_all/{z}/{x}/{y}.png"));

d3.json("rides_mls.json", function(error, collection) {
  if (error) throw error;

map.addLayer(new L.geoJSON(collection, {style : myStyle}));

});
