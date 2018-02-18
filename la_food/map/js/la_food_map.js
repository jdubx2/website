
var geojsonMarkerOptions = {
    radius: 8,
    fillColor: "#ff7800",
    color: "#000",
    weight: 1,
    opacity: 1,
    fillOpacity: 0.8
};

var markerColor = d3.scaleSequential(d3.interpolatePiYG)
  .domain([.1,.6]);

var map = new L.Map("map").setView([34.0022,-118.2437], 11);

map.addLayer(new L.TileLayer("https://cartodb-basemaps-{s}.global.ssl.fastly.net/dark_all/{z}/{x}/{y}.png"));

d3.json("geo.json", function(error, collection) {
  if (error) throw error;

L.geoJSON(collection, {

    pointToLayer: function (feature, latlng) {
        var fill_color = markerColor(feature.properties.weightedScore);
        return L.circleMarker(latlng, {radius: 4, fillColor: fill_color, fillOpacity: 0.65,
                                       color: "#000", weight : .2});
      },
      onEachFeature: function (feature, layer) {
        layer.bindPopup(feature.properties.yelpName);
         }

      }).addTo(map);
  });
