# BBCVoxelLandscape

Demonstration of voxel landscape for the BBC Micro. 

Based on the example code at https://github.com/s-macke/VoxelSpace/tree/master?tab=readme-ov-file

Assemble with beebasm
```
beebasm.exe -i <filename>.asm -do voxel.ssd -opt 2
```
Where <filename> is:
- voxasm : Mode 8 32K low resolution version 
- voxasm_sw : Mode 2 Sideways RAM high resolution version 
- voxTubeL : Second Processor low resolution version
- voxTubeH : Second Processor high resolution version
- voxTubeN : Second Processor Nula version

All version use double buffering to smooth animation and a fast multiply routine. Some versions use compression to allow more example landscapes on the disk. The low resolution versions average points along the horizontal to reduce memory footprint and calculation time. The line drawing has been updated to combine shorter line lengths together for more efficient drawing although this is less effective for the Nula version. The mode 8 version combines the odd/even lines together and draws them as pairs. The Nula has alternate palettes for maps A-D as maps I-L. For the second processor versions the speed is limited by the second processor so the faster the second processor the better the framerate. Timings below are given 3MHZ 6502.

Table of version differences:
|Version|Mode|Landscape|Colour|FPS|
|---|---|---|---|---|
|voxasm|8|64x128|Height Based|3.4|       
|voxasm_sw|2|128x128|Height Based|2.4|       
|voxTubeL|2|64x128|Height Based|4.3|
|voxTubeH|2|128x128|Height Based|3.6|
|voxTubeN|2|128x128|Texture Map|2.9|
