#!/bin/sh

for f in images/*_time.png; do
  convert $f -fill 'rgb(223,127,46)' -opaque '#0071bc' images/`basename $f _time.png`_EAS.png
  convert $f -fill 'rgb(206,18,73)' -opaque '#0071bc' images/`basename $f _time.png`_ECS.png
  convert $f -fill 'rgb(58,148,60)' -opaque '#0071bc' images/`basename $f _time.png`_LCN.png
  convert $f -fill 'rgb(127, 62, 131)' -opaque '#0071bc' images/`basename $f _time.png`_MEA.png
  convert $f -fill 'rgb(77, 77, 76)' -opaque '#0071bc' images/`basename $f _time.png`_NAC.png
  convert $f -fill 'rgb(32, 120, 182)' -opaque '#0071bc' images/`basename $f _time.png`_SAS.png
  convert $f -fill 'rgb(255, 203, 6)' -opaque '#0071bc' images/`basename $f _time.png`_SSF.png
done