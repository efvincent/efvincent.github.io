---
date: 2010-08-22 23:57:06
title: In Car Video–Carolina Motorsports Park
categories:
  - Functional
  - F#
tag:
  - Carolina Motorsports Park
  - CMP
  - DE
  - Driver Education
  - PCA
  - Porsche
---

I’ve just finished uploading a new in-car video from the Hurricane region PCA Driver’s Ed event at Carolina Motorsports Park in South Carolina. View it embedded here, or click the link below the picture to go to Vimeo.

[PCA DE CMP August 2010](http://vimeo.com/14339850) from [Eric Vincent](http://vimeo.com/efvincent) on [Vimeo](http://vimeo.com).

 <!-- more -->

Some notes about the video and data acquisition, for those interested in the gory, geeky details:

· Video was shot on a [GoPro HD](http://www.amazon.com/gp/product/B002VA57XC). Last year I tried a ContourHD, but the picture wasn’t quite as good and the sound was terrible – the GoPro does a better job dealing with the wind noise.

· The data (speedo, Tach, Throttle position) was collected using a [PLX 2340 KIWI Wifi](http://www.amazon.com/PLX-Devices-2340-KIWI-Wifi/dp/B002ICSOTC). This device interfaces with the OBD-II port of your car (also used by mechanics to read diagnostics), and transmits several different types of metrics in real time over Wi-Fi at with an observed sample rate of about 3 samples / second.

· The data was then recorded using [Rev by DevToaster](http://www.devtoaster.com/products/rev/) for iPhone. This app is compatible with the PLX, so it can collect data being transmitted by your car. It also adds accelerometer data. There’s GPS too, but the iPhone’s GPS is completely unusable for track purposes. The sampling frequency is too low, and it does some very funky interpolation which in the end renders the GPS data useless. For example, here’s a simple plot (graphed using F#) of one of the runs:

[![GPS Plot 100814-01](http://blog.efvincent.com/wp-content/uploads/2010/08/GPSPlot10081401_thumb.png)](http://blog.efvincent.com/wp-content/uploads/2010/08/GPSPlot10081401.png)

· The data is then exported from the phone as an emailed comma separated value file (.csv).

· A small program I wrote in F# reads the file and transforms the timestamps in to frame numbers (29.97 frames/second), and the data into either rotational data (for the analog gauges) or simple integers (for the digital gauges).

· That data can then be imported into Adobe After Effects. Images of the gauges (drawn from scratch in Adobe Illustrator) are animated into a video overlay using the PLX/Rev data.

· Adobe Premiere then takes the gauges and overlays them on top of the video from the track. There’s some fiddling involved to get the gauge data and video in sync. Premiere then renders the video (20 minutes took 6 hours... not sure if I’m doing that right).

· That rendered video is HD and is pretty big, so I used Microsoft Expression Encoder (Adobe has an encoder too) to re-encode the video into a smaller MP4 format that Vimeo wanted for uploading.
