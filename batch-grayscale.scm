; Batch convert RGB images to grayscale mode
(define (batch-convert-to-grayscale input-dir output-dir)
  (let* ((filelist '()))
    
    ; Print debug info about paths
    (gimp-message (string-append "Input directory: " input-dir))
    (gimp-message (string-append "Output directory: " output-dir))
    
    ; Create a dialog to show progress
    (gimp-progress-init "Converting Images to Grayscale Mode" -1)
    
    ; Helper function to normalize path (replace backslashes with forward slashes)
    (define (normalize-path path)
      (let* ((chars (string->list path))
             (fixed-chars (map (lambda (c) (if (char=? c #\\) #\/ c)) chars)))
        (list->string fixed-chars)))
    
    ; Helper function to get filename from path
    (define (get-filename-from-path filepath)
      (let* ((normalized-path (normalize-path filepath))
             (parts (strbreakup normalized-path "/")))
        (if (> (length parts) 0)
            (car (last parts))
            filepath)))
    
    ; For each file in the directory
    (let* ((current-files (cadr (file-glob (string-append input-dir "\\*.*") 1))))
      (gimp-message (string-append "Found " (number->string (length current-files)) " total files"))
      
      (let loop ((files current-files)
                 (count 0))
        (if (null? files)
            (gimp-message (string-append "Processed " (number->string count) " images"))
            (let* ((filename (car files))
                   (extension (string-downcase (car (last (strbreakup filename "."))))))
              
              ; Check if it's an image file
              (if (member extension '("jpg" "jpeg" "png" "tif" "tiff" "bmp"))
                  (begin
                    (gimp-message (string-append "Processing: " filename))
                    
                    ; Try to load the image
                    (let* ((image (car (gimp-file-load RUN-NONINTERACTIVE filename filename))))
                      (if (= image -1)
                          (gimp-message (string-append "Failed to load: " filename))
                          (let* ((; Get just the filename for output
                                 base-name (get-filename-from-path filename)))
                            
                            ; Convert the image to grayscale mode
                            (gimp-message "Converting to grayscale mode...")
                            (gimp-image-convert-grayscale image)
                            
                            ; Save to output directory
                            (let ((output-path (string-append output-dir "/" base-name))
                                  (drawable (car (gimp-image-get-active-layer image))))
                              (gimp-message (string-append "Saving to: " output-path))
                              (gimp-file-save RUN-NONINTERACTIVE image drawable output-path output-path))
                            
                            ; Close the image
                            (gimp-image-delete image)
                            
                            ; Update progress
                            (gimp-progress-update (/ count (length current-files)))
                            
                            ; Continue with next file
                            (loop (cdr files) (+ count 1))))))
                  
                  ; Not an image, skip to next file
                  (loop (cdr files) count))))))
    
    ; Finish up
    (gimp-progress-end)
    (gimp-displays-flush)))

; Register the script with GIMP
(script-fu-register
  "batch-convert-to-grayscale"                     ; func name
  "Batch Convert to Grayscale"                     ; menu label
  "Convert RGB images to true grayscale mode"      ; description
  "Your Name"                                      ; author
  "Copyright 2025"                                 ; copyright
  "2025"                                           ; date created
  ""                                               ; image type that the script works on
  SF-DIRNAME "Input Directory" ""
  SF-DIRNAME "Output Directory" ""
)

; Add to menu
(script-fu-menu-register "batch-convert-to-grayscale" "<Image>/File/Batch")