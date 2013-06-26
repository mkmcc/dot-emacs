;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby things
;;
(defvar tioga-prototype
"require 'Tioga/FigureMaker'
require 'plot_styles.rb'
require 'Dobjects/Function'

class MyPlots

  include Math
  include Tioga
  include FigureConstants
  include MyPlotStyles

  def t
    @figure_maker
  end

  def initialize
    @figure_maker = FigureMaker.default

    t.save_dir = 'plots'

    t.def_figure('NAME') do
      mnras_style
      enter_page
    end
  end

  def enter_page
    sans_serif_style
    mnras_style

    t.default_frame_left   = 0.12
    t.default_frame_right  = 0.98
    t.default_frame_top    = 0.96
    t.default_frame_bottom = 0.12

    t.default_page_width  = 72 * 3.5

    t.default_page_height = t.default_page_width * \\
      (t.default_frame_right - t.default_frame_left) / \\
      (t.default_frame_top - t.default_frame_bottom)

    t.default_enter_page_function
  end

end

MyPlots.new
"
"starting point for tioga files")

(defun tioga-start ()
  (interactive)
  (erase-buffer)
  (insert tioga-prototype))

(provide 'mkmcc-ruby)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
