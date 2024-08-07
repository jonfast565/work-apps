#lang racket

(require racket/cmdline
         racket/path
         racket/list
         racket/file)

;; Check if a path is a file
(provide/contract
 [is-file? (path? . -> . boolean?)])
(define (is-file? path)
  (eq? (file-or-directory-type path) 'file))

;; Check if a path is a directory
(provide/contract
 [is-directory? (path? . -> . boolean?)])
(define (is-directory? path)
  (eq? (file-or-directory-type path) 'directory))

;; Read a file to a string
(provide/contract
 [read-file (path? . -> . string?)])
(define (read-file path)
  (fprintf (current-output-port) "Reading file ~s\n" (path->string path))
  (file->string path #:mode 'text))

;; Check if a path contains certain parts, passed as a list
;; Can be used to exclude certain folders from getting scanned
(provide/contract
 [path-contains? (-> path? (listof path?) boolean?)])
(define (path-contains? path path-parts)
  (for/or ([i (explode-path path)])
    (for/or ([j path-parts])
      (string=? (path->string i) (path->string j)))
    )
  )

;; Walk a directory
;; Requires a directory, extension to look for, and a list of folder path parts to exclude
(provide/contract
 [walk-directory (path? string? (listof path?) . -> . sequence?)])
(define (walk-directory dir extension exclude-folders)
  (let ([fileRegex (regexp (string-append "^.*\\." extension "$"))]
        [directoryCursor (in-directory dir)])
    (sequence-filter (lambda (p)
                       (and (regexp-match? fileRegex p) (not (path-contains? p exclude-folders)))) directoryCursor))
  )

;; Recursively walks a directory
;; Requires a directory, extension, and a list of folders to exclude,
;; Returns a list of read files that were read from all folders and subfolders 
(provide/contract
 [rec-directory (-> string? string? (listof string?) list?)])
(define (rec-directory dir extension exclude-folders)
  (define (rec-directory-helper dir extension exclude-folders results)
    (let* ([paths (walk-directory dir extension exclude-folders)]
           [files (sequence-filter is-file? paths)]
           [dirs (sequence-filter is-directory? paths)]
           [fileContents (sequence-map (lambda (f) (read-file f)) files)]
           [dirPaths (sequence-map (lambda (d) (rec-directory-helper d extension exclude-folders results)) dirs)])
      (sequence-append results fileContents (flatten-list dirPaths))
      )
    )
  (rec-directory-helper dir extension exclude-folders '())
  )

;; Flattens a list of any item with any depth to a 1d list
;; Doesn't matter how deep the list is, this will flatten it
(provide/contract
 [flatten-list (-> list? list?)])
(define (flatten-list lst)
  (define (flatten-helper item acc)
    (cond
      [(string? item) (cons item acc)]
      [(path? item) (cons item acc)]
      [(sequence? item) (for/fold ([a acc]) ([i item]) (flatten-helper i a))]
      [(list? item) (foldl (lambda (i a) (flatten-helper i a)) acc item)]
      [else (error "Unsupported type: ~a" item)]))

  (reverse (flatten-helper lst '())))

;; Define command line parameters
(define file-extension (make-parameter "cs"))
(define search-directory (make-parameter (string->path "C:\\Users\\jfast\\Desktop\\SqlCodeGenerator") #| (current-directory) |#))
(define exclude-folders (make-parameter "bin,obj"))
(define output-folder (make-parameter (current-directory)))
(define output-filename (make-parameter "merged-file"))

;; Parse command-line arguments
(define (parse-command-line-arguments)
  (command-line
   #:program "SqlCodeGenerator"
   #:once-each
   [("-e" "--extension") extension "File extension to search for"
                         (file-extension extension)]
   [("-d" "--directory") directory "Directory to search"
                         (search-directory (string->path directory))]
   [("-x" "--exclude") exclude "Comma-separated list of folders to exclude (in quotes if necessary)"
                       (exclude-folders (map string->path (string-split exclude ",")))]
   [("-o" "--output-folder") folder "Output folder"
                             (output-folder (string->path folder))]
   [("-f" "--output-filename") filename "Output filename"
                               (output-filename filename)]
   )

  ;; check for nulls before running
  (if (null? (file-extension)) (error "File extension not provided") null)
  (if (null? (search-directory)) (error "Search directory not provided") null)

  ;; Print the parsed arguments for demonstration
  (printf "--- Concatenator! ---\n\n")
  (printf "File extension: ~a\n" (file-extension))
  (printf "Search directory: ~a\n" (search-directory))
  (printf "Exclude folders: ~a\n" (exclude-folders))
  (printf "Output folder: ~a\n" (output-folder))
  (printf "Output filename: ~a.~a\n\n" (output-filename) (file-extension))
  )

;; Get the output path of the file (dubious)
(define output-path
   (build-path (output-folder) (string-append (output-filename) "." (file-extension))))

;; Main method
(define (main)
  (parse-command-line-arguments)
  (printf "--- Files Read ---\n\n")
  (let* ([search-directory (search-directory)]
         [file-extension (file-extension)]
         [exclude-folders-list (map string->path (string-split (exclude-folders) ","))]
         [results (flatten-list (rec-directory search-directory file-extension exclude-folders-list))]
         [concatted (string-join results "\n")])
    (printf "\n--- Wrote File ~s ---\n\n" (path->string output-path))
    (display-to-file concatted output-path #:mode 'text #:exists 'truncate/replace)
    )
  )

(main)
