<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link rel="icon" type="image/x-icon" href="assets/favicon.ico">
    <title>Sphere Utility</title>
    <link rel="stylesheet" href="app.css">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/css/bootstrap.min.css" integrity="sha384-rwoIResjU2yc3z8GV/NPeZWAv56rSmLldC3R/AZzGRnGxQQKnKkoFVhFQhNUwEyJ" crossorigin="anonymous">
  </head>
  <body>
    <div id="elm-area"></div>
    <script src="https://maps.googleapis.com/maps/api/js?key=<?php include 'my-key.local'; ?>"
    async defer></script>
    <script src="bundle.js"></script>
    <script>
      var app = Elm.Main.embed(document.getElementById("elm-area"));
      app.ports.setPoint.subscribe(function(point) {
        var map = new google.maps.Map(document.getElementById('map'), {
          zoom: 5,
          center: point,
          mapTypeId: 'terrain'
        });
        var marker = new google.maps.Marker({
          position: point,
          map: map,
        });
      });
      app.ports.setDistance.subscribe(function(distObj) {
        var map = new google.maps.Map(document.getElementById('distanceMap'), {
          zoom: 3,
          center: distObj.d1,
          mapTypeId: 'terrain'
        });
        var marker = new google.maps.Marker({
          position: distObj.d1,
          Label: 'A',
          map: map,
        });
        var marker = new google.maps.Marker({
          position: distObj.d2,
          Label: 'B',
          map: map,
        });
        var path = [
          distObj.d1,
          distObj.d2
        ];
        var distPath = new google.maps.Polyline({
          path: path,
          geodesic: true,
          strokeColor: '#FF0000',
          strokeOpacity: 1.0,
          strokeWeight: 2
        });
        distPath.setMap(map);
      });
    </script>
  </body>
</html>
