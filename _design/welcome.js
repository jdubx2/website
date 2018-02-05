
var path = d3.selectAll('.svg_letter')
.attr('fill', 'none')
.attr('stroke', 'black');

var totalLength = path.node().getTotalLength();

path.attr("stroke-dasharray", totalLength + " " + totalLength)
  .attr("stroke-dashoffset", totalLength)
  .transition()
    .duration(4000)
    .ease(d3.easeSin)
    .attr("stroke-dashoffset", 0)
  .transition()
    .duration(800)
    .attr('fill', 'black')
  .transition();
