#pragma once

#include <cstdint>

struct Pixel {
  uint8_t red;
  uint8_t green;
  uint8_t blue;

  /**
   * Tells if two pixels are equal by colors.
   * @param p - second pixel
   * @return Bool
   */
  [[nodiscard]] bool are_equal(Pixel p) const;
};
