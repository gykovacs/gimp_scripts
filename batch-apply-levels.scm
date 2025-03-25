; Batch apply levels adjustment to images
(define (batch-apply-levels-simple input-dir output-dir in-low in-high gamma out-low out-high)
  (let* ((filelist '()))
    
    ; Print debug info about paths
    (gimp-message (string-append "Input directory: " input-dir))
    (gimp-message (string-append "Output directory: " output-dir))
    
    ; Create a dialog to show progress
    (gimp-progress-init "Batch Processing Images" -1)
    
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
                          (let* ((drawable (car (gimp-image-get-active-layer image)))
                                 ; Get just the filename for output
                                 (base-name (get-filename-from-path filename)))
                            
                            ; Apply the levels adjustment
                            (gimp-levels drawable HISTOGRAM-VALUE in-low in-high gamma out-low out-high)
                            
                            ; Save to output directory
                            (let ((output-path (string-append output-dir "/" base-name)))
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
  "batch-apply-levels-simple"                       ; func name
  "Batch Apply Levels (Simple)"                     ; menu label
  "Apply levels adjustment to images in a folder"   ; description
  "Your Name"                                       ; author
  "Copyright 2025"                                  ; copyright
  "2025"                                            ; date created
  ""                                                ; image type that the script works on
  SF-DIRNAME "Input Directory" ""
  SF-DIRNAME "Output Directory" ""
  SF-ADJUSTMENT "Input Black Point" '(0 0 255 1 10 0 0)
  SF-ADJUSTMENT "Input White Point" '(255 0 255 1 10 0 0)
  SF-ADJUSTMENT "Gamma" '(1.0 0.1 10.0 0.1 1.0 1 1)
  SF-ADJUSTMENT "Output Black Point" '(0 0 255 1 10 0 0)
  SF-ADJUSTMENT "Output White Point" '(255 0 255 1 10 0 0)
)

; Add to menu
(script-fu-menu-register "batch-apply-levels-simple" "<Image>/File/Batch")