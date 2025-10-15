//
// Uncomment to enable native Pascal mp3 layer3 decoder, which do not require external dependencies
{$define ACS_MP3IN_L3_BUILTIN}

//
// Uncomment to enable external mp3 layer1-2 decoders for non-Windows builds, which DO require external dependencies
//{$define ACS_MP3IN_L12_EXT}

//
// Uncomment to enable native Pascal vorbis encoders/decoders, which no NOT require external dependencies
{$define ACS_VORBIS_BUILTIN}

//
// Uncomment to enable external vorbis encoders/decoders, which DO require external dependencies
//{$define ACS_VORBIS_EXT}

//
// Uncomment to enable built-in FLAC decoder, which do not require external dependencies
{$define ACS_FLAC}

//
// Uncomment to enable external flac encoders/decoders, which DO require external dependencies
//{$define ACS_FLAC_EXT}

//
// Uncomment to enable external lame encoders/decoders, which DO require external dependencies
{$define ACS_LAME_EXT}

//
// Uncomment to enable native Pascal wav encoders/decoders, which do not require external dependencies
{$define ACS_WAVE_BUILTIN}