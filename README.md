# sophisticated-clipboard

sophisticated-clipboard let access system clipboard.

## Installation

```
(ql:quickload :sophisticated-clipboard)
```

## Dependencies
- CFFI (only on Windows)

## Usage

get text from clipboard.

```
(sophisticated-clipboard:text)
```

put text to clipboard.

```
(sophisticated-clipboard:text "put text!")
```

## Author
sophisticated fork: Lukáš Hozda (me@mag.wiki)
original: SANO Masatoshi (snmsts@gmail.com)

## Project
 * https://github.com/luciusmagn/sophisticated-clipboard

## License
Licensed under the MIT License.
