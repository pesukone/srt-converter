# srt-converter

Usage:

```
stack run input.srt output.srt
```

The file needs to be UTF-8 encoded, and have Unix line endings. ``dos2unix`` should convert the line endings, and ``iconv`` can be used for encoding conversion. To check the file format, use the command ``file``:

```
$ file input.srt
input.srt: SubRip, Unicode text, UTF-8 text
```
