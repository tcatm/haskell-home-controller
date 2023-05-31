packages = \
	-package hashable \
	-package monad-logger \
	-package network \
	-package pretty-show \
	-package conduit \
	-package async \
	-package conduit-extra \
	-package mtl \
	-package split \
	-package aeson \
	-package yesod \
	-package yesod-static \
	-package unordered-containers \
	-package scientific \
	-package vector \
	-package connection \
	-package http-client-tls \
	-package http-client \
	-package ini \
	-package stm

sources = \
	APDU.hs \
	BlindsDevice.hs \
	Console.hs \
	Device.hs \
	DeviceRunner.hs \
	DPTs.hs \
	ElphiWohnung.hs \
	FM2.hs \
	KNXAddress.hs \
	KNXDatatypes.hs \
	KNX.hs \
	KNXMessages.hs \
	KNXTelegram.hs \
	StaircaseLight.hs \
	TimeSender.hs \
	Webinterface.hs \
	Hue/Hue.hs \
	Main.hs

Main: $(sources)
	ghc -o Main Main.hs $(packages)
