var dimension = 920;
$(document).on("shiny:sessioninitialized", function(event) { //shiny:sessioninitialized
    dimension = document.getElementById('forestbox').offsetWidth;
    Shiny.onInputChange("dimension", dimension);
});
$(window).resize(function(event) {
    dimension = document.getElementById('forestbox').offsetWidth;
    Shiny.onInputChange("dimension", dimension);
});
/*test for javascript to run again when clicking plot tab

 ATTACH ID TO PLOT TAB
 THEN REPLACE myBtn with ID
 ADD DIMENSION CHANGE


document.getElementById("plotButton").addEventListener("click", plotDimension);
function plotDimension() {
  dimension = document.getElementById('forestbox').offsetWidth;
  Shiny.onInputChange("dimension", dimension);
}
*/