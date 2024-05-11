# BBCVoxelLandscape

Demonstration of voxel landscape for the BBC Micro. 

Based on the example code at https://github.com/s-macke/VoxelSpace/tree/master?tab=readme-ov-file

Assemble with beebasm
```
beebasm.exe -i <filename>.asm -do voxel.ssd -opt 2 -title Voxel
```
Where <filename> is:
- voxasm : Mode 8 32K low resolution version 
- voxTubeL : Second Processor low resolution version
- voxTubeH : Second Processor high resolution version
- voxTubeN : Second Processor Nula version

All version use double buffering to smooth animation and a fast multiply routine. Some versions use compression to allow more example landscapes on the disk. The low resolution versions average points along the horizontal to reduce memory footprint and calculation time. The higher resolution versions combine lines of the same colour together to reduce draw overhead although this is less effective for the Nula version. The Nula has alternate palettes for maps A-D as maps I-L. For the second processor versions the speed is limited by the second processor so the faster the second processor the better the framerate.

Table of version differences:
|Version|Mode|Landscape|Colour|Line drawing|
|---|---|---|---|---|
|voxasm|8|64x128|Height Based|Per Voxel|       
|voxTubeL|2|64x128|Height Based|Per Voxel|
|voxTubeH|2|128x128|Height Based|Combined|
|voxTubeN|2|128x128|Texture Map|Combined|
